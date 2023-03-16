use std::collections::HashMap;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_syntax::node::db::SyntaxGroup;

const IMPLICIT_ATTR: &str = "implicit";
const WARPMEMORY_TYPE: &str = "DictFelt252To<u128>";
const WARPMEMORY_NAME: &str = "warp_memory";

pub fn handle_implicits(
    db: &dyn SyntaxGroup,
    function_ast: &ast::FunctionWithBody,
) -> (Option<RewriteNode>, Vec<PluginDiagnostic>) {
    let mut diagnostics = vec![];
    let mut arguments = vec![];
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
                            arguments.push(format!(
                                "ref {arg_name}: {WARPMEMORY_TYPE}, "
                            ));
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
        return (None, diagnostics);
    }

    if arguments.is_empty() {
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
                    RewriteNode::new_trimmed(
                        function_ast.body(db).statements(db).as_syntax_node(),
                    ),
                ),
            ]),
        )),
        diagnostics
    )
}