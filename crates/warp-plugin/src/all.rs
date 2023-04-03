use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_syntax::node::ast::{
    ArgListParenthesized, Expr, ExprBlock, ExprFunctionCall, FunctionDeclaration,
    FunctionSignature, FunctionWithBody, MaybeModuleBody, ModuleBody, Statement,
};
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::{ast, db::SyntaxGroup, Terminal, TypedSyntaxNode};
use itertools::Itertools;
use smol_str::SmolStr;
use std::collections::HashMap;

use crate::kind::IsExpression;
use std::string::String;

const STATEMENT: &str = "statement";
const IMPLICIT_ATTR: &str = "implicit";
const WARPMEMORY_NAME: &str = "warp_memory";
const WARPMEMORY_TYPE: &str = "Felt252Dict<u128>";

type HandlingResult = (RewriteNode, Vec<PluginDiagnostic>);
type FuncName = SmolStr;
type Implicits = Vec<String>;

fn handle_module(
    db: &dyn SyntaxGroup,
    module_body: ModuleBody,
    name: SmolStr,
    attributes: SyntaxNode,
) -> HandlingResult {
    let mut kept_original_items = vec![];
    let mut modified_functions = vec![];
    let mut modified_modules = vec![];
    let mut diagnostics = vec![];

    let function_with_implicits = gather_function_with_implicits(db, &module_body);

    module_body
        .items(db)
        .elements(db)
        .into_iter()
        .foreach(|item| match item {
            ast::Item::FreeFunction(function_body) => {
                let (rewritten_function, item_diagnostic) =
                    handle_function(db, &function_with_implicits, function_body);
                modified_functions.push(rewritten_function);
                diagnostics.extend(item_diagnostic);
            }
            ast::Item::Module(child_module) => {
                let name = child_module.name(db).text(db);
                let attributes = child_module.attributes(db).as_syntax_node();
                if let MaybeModuleBody::Some(body) = child_module.body(db) {
                    let (rewritten_module, item_diagnostic) =
                        handle_module(db, body, name, attributes);
                    modified_modules.push(rewritten_module);
                    diagnostics.extend(item_diagnostic);
                } else {
                    kept_original_items.push(RewriteNode::Copied(child_module.as_syntax_node()));
                }
            }
            _ => kept_original_items.push(RewriteNode::Copied(item.as_syntax_node())),
        });

    (
        RewriteNode::interpolate_patched(
            "
                $attributes$
                mod $name$ {
                    $original_items$
                    $modified_functions$
                    $modified_modules$
                }
                ",
            HashMap::from([
                (
                    "attributes".to_string(),
                    RewriteNode::new_trimmed(attributes),
                ),
                ("name".to_string(), RewriteNode::Text(name.to_string())),
                (
                    "original_items".to_string(),
                    RewriteNode::new_modified(kept_original_items),
                ),
                (
                    "modified_functions".to_string(),
                    RewriteNode::new_modified(modified_functions),
                ),
                (
                    "modified_modules".to_string(),
                    RewriteNode::new_modified(modified_modules),
                ),
            ]),
        ),
        diagnostics,
    )
}

fn handle_function(
    db: &dyn SyntaxGroup,
    function_with_implicits: &HashMap<FuncName, Implicits>,
    function_body: FunctionWithBody,
) -> HandlingResult {
    let (rewritten_expr_bloc, expr_block_diagnostics) =
        handle_expression_blocks(db, function_with_implicits, function_body.body(db));

    let (rewritten_declaration, declaration_diagnostics) =
        handle_function_declaration(db, &function_body);

    let rewritten_function = RewriteNode::interpolate_patched(
        "$func_decl$ {
            $body$
        }",
        HashMap::from([
            ("func_decl".to_string(), rewritten_declaration),
            ("body".to_string(), rewritten_expr_bloc),
        ]),
    );
    let mut func_diagnostics = expr_block_diagnostics;
    func_diagnostics.extend(declaration_diagnostics);

    (rewritten_function, func_diagnostics)
}

fn handle_function_declaration(
    db: &dyn SyntaxGroup,
    func_body: &FunctionWithBody,
) -> HandlingResult {
    let implicit_attr = func_body
        .attributes(db)
        .elements(db)
        .into_iter()
        .find(|attr| attr.attr(db).text(db) == IMPLICIT_ATTR);
    if (implicit_attr == None) {
        return (RewriteNode::from_ast(&func_body.declaration(db)), vec![]);
    }
    let implicit_attr = implicit_attr.unwrap();

    let mut diagnostics: Vec<PluginDiagnostic> = vec![];
    let mut parameters: Vec<String> = vec![];
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
        return (
            RewriteNode::from_ast(&func_body.declaration(db)),
            diagnostics,
        );
    }

    let mut func_declaration = RewriteNode::from_ast(&func_body.declaration(db));
    func_declaration
        .modify_child(db, FunctionDeclaration::INDEX_SIGNATURE)
        .modify_child(db, FunctionSignature::INDEX_PARAMETERS)
        .modify(db)
        .children
        .as_mut()
        .unwrap()
        .insert(0, RewriteNode::Text(parameters.join("")));

    (func_declaration, diagnostics)
}

fn handle_expression_blocks(
    db: &dyn SyntaxGroup,
    function_with_implicits: &HashMap<FuncName, Implicits>,
    expr_block: ExprBlock,
) -> HandlingResult {
    let mut diagnostics: Vec<PluginDiagnostic> = vec![];
    let mut rewrite_body: HashMap<String, RewriteNode> = HashMap::new();

    // Rewrite each statment of the expression block
    let mut index = 0;
    for statement in expr_block.statements(db).elements(db) {
        let (rewrite_statement, statement_diagnostics) =
            handle_statement(db, function_with_implicits, statement);

        if statement_diagnostics.len() > 0 {
            diagnostics.extend(statement_diagnostics);
        }

        let statement_key = format!("{STATEMENT}_{index}");
        rewrite_body.insert(statement_key, rewrite_statement);
        index += 1;
    }

    // Rewrite the expression block
    let expr_block_str = (0..index).map(|i| format!("${STATEMENT}_{i}$")).join("\n");
    let expr_bloc_rewrriten = RewriteNode::interpolate_patched(&expr_block_str, rewrite_body);

    (expr_bloc_rewrriten, diagnostics)
}

fn handle_statement(
    db: &dyn SyntaxGroup,
    function_with_implicits: &HashMap<FuncName, Implicits>,
    statement: Statement,
) -> HandlingResult {
    match statement {
        Statement::Expr(stmnt) => {
            let (rewritten_expr, diagnostics) =
                handle_expression(db, function_with_implicits, stmnt.expr(db));
            (
                RewriteNode::interpolate_patched(
                    "$expr$;",
                    HashMap::from([("expr".to_string(), rewritten_expr)]),
                ),
                diagnostics,
            )
        }
        Statement::Let(stmnt) => {
            let (rewritten_expr, diagnostics) =
                handle_expression(db, function_with_implicits, stmnt.rhs(db));

            let mut rewritten_stmnt = RewriteNode::from_ast(&stmnt);
            let children = rewritten_stmnt.modify(db).children.as_mut().unwrap();
            children[ast::StatementLet::INDEX_RHS] = rewritten_expr;
            (rewritten_stmnt, diagnostics)
        }
        Statement::Missing(_stmnt) => todo!(),
        Statement::Return(stmnt) => {
            let (rewritten_expr, diagnostics) =
                handle_expression(db, function_with_implicits, stmnt.expr(db));
            (
                RewriteNode::interpolate_patched(
                    "return $expr$;",
                    HashMap::from([("expr".to_string(), rewritten_expr)]),
                ),
                diagnostics,
            )
        }
    }
}

fn handle_expression(
    db: &dyn SyntaxGroup,
    function_with_implicits: &HashMap<FuncName, Implicits>,
    expression: Expr,
) -> HandlingResult {
    let children = expression.as_syntax_node().children(db);

    let mut chidlren_to_modify: Vec<(usize, RewriteNode)> = vec![];
    let mut diagnostics: Vec<PluginDiagnostic> = vec![];

    for (index, child) in children.enumerate() {
        if !child.kind(db).is_expression() {
            continue;
        }
        let expr = Expr::from_syntax_node(db, child);

        let (rewritten_child, child_diagnostic) = match expr {
            Expr::Block(block_expr) => {
                handle_expression_blocks(db, function_with_implicits, block_expr)
            }
            Expr::FunctionCall(func_call) => {
                let func_name = get_func_call_name(db, &func_call);

                let (mut rewritten_call, func_call_diagnostics) =
                    handle_expression(db, function_with_implicits, Expr::FunctionCall(func_call));
                if let Some(custom_implicits) = function_with_implicits.get(&func_name) {
                    let arg_children = rewritten_call
                        .modify_child(db, ExprFunctionCall::INDEX_ARGUMENTS)
                        .modify_child(db, ArgListParenthesized::INDEX_ARGS)
                        .modify(db)
                        .children
                        .as_mut()
                        .unwrap();
                    for implicit in custom_implicits.iter().rev() {
                        arg_children.insert(0, RewriteNode::Text(implicit.clone()));
                    }
                }
                (rewritten_call, func_call_diagnostics)
            }
            _ => handle_expression(db, function_with_implicits, expr),
        };

        diagnostics.extend(child_diagnostic);
        chidlren_to_modify.push((index, rewritten_child));
    }

    let mut rewritten_expression = RewriteNode::from_ast(&expression);
    let children_to_rewrite = rewritten_expression.modify(db).children.as_mut().unwrap();
    for (index, child_to_rewrite) in chidlren_to_modify {
        children_to_rewrite[index] = child_to_rewrite;
    }

    return (rewritten_expression, diagnostics);
}

fn gather_function_with_implicits(
    db: &dyn SyntaxGroup,
    module_body: &ModuleBody,
) -> HashMap<FuncName, Implicits> {
    module_body.items(db).elements(db).into_iter().fold(
        HashMap::new(),
        |mut name_to_implicit, item| match item {
            ast::Item::FreeFunction(func) => {
                let custom_implicts = func
                    .attributes(db)
                    .elements(db)
                    .into_iter()
                    .find(|attr| attr.attr(db).text(db) == IMPLICIT_ATTR);
                if let Some(custom_implicts) = custom_implicts {
                    if let ast::OptionAttributeArgs::AttributeArgs(args) = custom_implicts.args(db)
                    {
                        let implicit_params = args.arg_list(db).elements(db).into_iter().fold(
                            vec![],
                            |mut acc, arg| {
                                let arg_name = get_implicit_arg(db, arg).unwrap();
                                acc.push(format!("ref {arg_name}: {WARPMEMORY_TYPE}, "));
                                acc
                            },
                        );
                        let func_name = func.declaration(db).name(db).text(db);
                        name_to_implicit.insert(func_name, implicit_params);
                    }
                }

                name_to_implicit
            }
            _ => name_to_implicit,
        },
    )
}

fn get_implicit_arg(db: &dyn SyntaxGroup, arg: Expr) -> Option<SmolStr> {
    if let ast::Expr::Path(expr) = arg {
        if let [ast::PathSegment::Simple(segment)] = &expr.elements(db)[..] {
            return Some(segment.ident(db).text(db));
        }
    }
    None
}

fn get_func_call_name(db: &dyn SyntaxGroup, func_call: &ExprFunctionCall) -> SmolStr {
    let path = func_call.path(db);
    if let [ast::PathSegment::Simple(func_name_token)] = &path.elements(db)[..] {
        return func_name_token.ident(db).text(db);
    }
    panic!("Couldn't get function call name");
}
