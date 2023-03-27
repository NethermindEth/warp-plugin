use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use smol_str::SmolStr;
use std::collections::HashMap;

const IMPLICIT_ATTR: &str = "implicit";
const WARPMEMORY_TYPE: &str = "Felt252Dict<u128>";
const WARPMEMORY_NAME: &str = "warp_memory";
const WARPMEMORY_IMPORT: &str = "use warplib::memory::WarpMemoryTrait;";

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
) -> (
    Option<RewriteNode>,
    HashMap<SmolStr, String>,
    Vec<PluginDiagnostic>,
) {
    let mut diagnostics = vec![];
    let mut arguments = vec![];
    let mut imports = HashMap::new();
    let _declaration = function_ast.declaration(db);
    let attributes = function_ast.attributes(db);

    for attr in attributes.elements(db) {
        if attr.attr(db).text(db) == IMPLICIT_ATTR {
            if let ast::OptionAttributeArgs::AttributeArgs(args) = attr.args(db) {
                for arg in args.arg_list(db).elements(db) {
                    if let ast::Expr::Path(expr) = arg {
                        if let [ast::PathSegment::Simple(segment)] = &expr.elements(db)[..] {
                            let arg_name = segment.ident(db).text(db);
                            // let arg_type = self.implicit_types.get(&arg_name).unwrap_or_else(|| panic!("Unknown implicit argument: {}", arg_name));
                            if arg_name != WARPMEMORY_NAME {
                                diagnostics.push(PluginDiagnostic {
                                    stable_ptr: expr.stable_ptr().untyped(),
                                    message: "Only warp_memory is supported.".into(),
                                });
                            }
                            arguments.push(format!("ref {arg_name}: {WARPMEMORY_TYPE}, "));
                            imports.insert(arg_name, WARPMEMORY_IMPORT.into());
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
                    stable_ptr: attr.stable_ptr().untyped(),
                    message: "Expected arguments.".into(),
                });
            }
        }
    }

    if !diagnostics.is_empty() {
        return (None, imports, diagnostics);
    }

    if arguments.is_empty() {
        return (None, imports, diagnostics);
    }

    let mut func_declaration = RewriteNode::from_ast(&function_ast.declaration(db));
    func_declaration
        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS)
        .modify(db)
        .children
        .as_mut()
        .unwrap()
        .insert(0, RewriteNode::Text(arguments.join("")));

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
        imports,
        diagnostics,
    )
}
