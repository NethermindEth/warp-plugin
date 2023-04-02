use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_semantic::ExprFunctionCall;
use cairo_lang_syntax::node::ast::{FunctionWithBody, ModuleBody};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};

use crate::function_calls::handle_function_calls;

const IMPLICIT_ATTR: &str = "implicit";
const WARPMEMORY_TYPE: &str = "Felt252Dict<u128>";
const WARPMEMORY_NAME: &str = "warp_memory";
// const WARPMEMORY_IMPORT: &str = "use warplib::memory::WarpMemoryTrait;";

fn gather_function_with_implicits(
    db: &dyn SyntaxGroup,
    module_body: ModuleBody,
) -> HashMap<SmolStr, FunctionWithBody> {
    let implict_funcs =
        module_body
            .items(db)
            .elements(db)
            .into_iter()
            .filter_map(|node| match node {
                ast::Item::FreeFunction(func) => {
                    if func
                        .attributes(db)
                        .elements(db)
                        .iter()
                        .any(|attr| attr.attr(db).text(db) == IMPLICIT_ATTR)
                    {
                        Some(func)
                    } else {
                        None
                    }
                }
                _ => None,
            });

    let mut func_map = HashMap::new();
    for func in implict_funcs {
        func_map.insert(func.declaration(db).name(db).text(db), func);
    }
    func_map
}

fn handle_everything(db: &dyn SyntaxGroup, module_body: ModuleBody) {
    let all_funcs = module_body
        .items(db)
        .elements(db)
        .into_iter()
        .filter_map(|node| match node {
            ast::Item::FreeFunction(f) => Some(f),
            _ => None,
        });
    let implicit_funcs = gather_function_with_implicits(db, module_body);

    let y: Vec<FunctionWithBody> = implicit_funcs.into_values().map(|x| x).collect();
}

//add corresponding arguments
fn handle_calls(
    db: &dyn SyntaxGroup,
    func_with_implicits: &HashSet<SmolStr>,
    function_ast: &ast::FunctionWithBody,
) {
    let function_calls = handle_function_calls(db, function_ast).iter().filter(|x| {
        let path = x.path(db);
        match &path.elements(db)[..] {
            [ast::PathSegment::Simple(name)] => {
                func_with_implicits.contains(&(name.ident(db).text(db)))
            }
            _ => false,
        }
    });
}

/// Handles the implicits for a Cairo function, returning a tuple containing an optional
/// `RewriteNode`, a `HashMap` of implicit function names to required import statements, and a
/// vector of any diagnostic messages.
///
/// The function will extract any arguments with an `#[implicit]` attribute and generate import
/// statements for them. If no arguments are found or if there are any diagnostic messages, the
/// function will return `None`.
///
/// For now, only `warp_memory` is supported.
/// # Arguments
///
/// * `db` - A reference to a `SyntaxGroup` trait object that contains the function's syntax tree.
/// * `function_ast` - The `FunctionWithBody` representing the function.
///
/// # Returns
///
/// A tuple containing an optional `RewriteNode`, a `HashMap` of implicit function names to import
/// statements, and a vector of any diagnostic messages.
///
pub fn handle_implicits(
    db: &dyn SyntaxGroup,
    function_ast: &ast::FunctionWithBody,
) -> (Option<RewriteNode>, Vec<PluginDiagnostic>) {
    let mut diagnostics = vec![];
    let mut parameters = vec![];

    let implicit_attr = function_ast
        .attributes(db)
        .elements(db)
        .into_iter()
        .find(|attr| attr.attr(db).text(db) == IMPLICIT_ATTR)
        .unwrap_or_else(|| panic!("Cannot find implict attribute"));

    let implicit_args = match implicit_attr.args(db) {
        ast::OptionAttributeArgs::AttributeArgs(args) => Some(args),
        _ => None,
    };

    if let ast::OptionAttributeArgs::AttributeArgs(args) = implicit_attr.args(db) {
        for arg in args.arg_list(db).elements(db) {
            if let ast::Expr::Path(expr) = arg {
                if let [ast::PathSegment::Simple(segment)] = &expr.elements(db)[..] {
                    let arg_name = segment.ident(db).text(db);
                    if arg_name != WARPMEMORY_NAME {
                        diagnostics.push(PluginDiagnostic {
                            stable_ptr: expr.stable_ptr().untyped(),
                            message: "Only warp_memory is supported.".into(),
                        });
                        continue;
                    }
                    parameters.push(format!("ref {arg_name}: {WARPMEMORY_TYPE}, "));
                } else {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: expr.stable_ptr().untyped(),
                        message: "Expected a single segment.".into(),
                    });
                }
            } else {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: arg.stable_ptr().untyped(),
                    message: "Expected path.".into(),
                });
            }
        }
    } else {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: implicit_attr.stable_ptr().untyped(),
            message: "Expected arguments.".into(),
        });
    }

    if !diagnostics.is_empty() || parameters.is_empty() {
        return (None, diagnostics);
    }

    let mut func_declaration = RewriteNode::from_ast(&function_ast.declaration(db));
    func_declaration
        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS)
        .modify(db)
        .children
        .as_mut()
        .unwrap()
        .insert(0, RewriteNode::Text(parameters.join("")));

    (
        Some(RewriteNode::interpolate_patched(
            "
                    $func_decl$ {
                        $body$
                    }
                ",
            HashMap::from([
                ("func_decl".to_string(), func_declaration),
                (
                    "body".to_string(),
                    RewriteNode::new_trimmed(function_ast.body(db).statements(db).as_syntax_node()),
                ),
            ]),
        )),
        diagnostics,
    )
}
