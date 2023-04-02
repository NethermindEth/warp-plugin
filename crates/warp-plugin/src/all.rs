use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_syntax::node::ast::{
    Expr, ExprBlock, FunctionDeclaration, FunctionSignature, FunctionWithBody, ModuleBody,
    Statement,
};
use cairo_lang_syntax::node::{ast, db::SyntaxGroup, Terminal, TypedSyntaxNode};
use itertools::Itertools;
use std::collections::HashMap;

const STATEMENT: &str = "statement";
const IMPLICIT_ATTR: &str = "implicit";
const WARPMEMORY_NAME: &str = "warp_memory";
const WARPMEMORY_TYPE: &str = "Felt252Dict<u128>";

type HandlingResult = (RewriteNode, Vec<PluginDiagnostic>);
type FuncName = String;
type Implicit = String;

fn handle_module(db: &dyn SyntaxGroup, module_body: ModuleBody) -> HandlingResult {
    todo!();
}

fn handle_function(
    db: &dyn SyntaxGroup,
    function_with_implicits: &HashMap<FuncName, Implicit>,
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

    if (!diagnostics.is_empty() || parameters.is_empty()) {
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
    function_with_implicits: &HashMap<FuncName, Implicit>,
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
    function_with_implicits: &HashMap<FuncName, Implicit>,
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
        Statement::Missing(stmnt) => todo!(),
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
    function_with_implicits: &HashMap<FuncName, Implicit>,
    expression: Expr,
) -> HandlingResult {
    todo!();
}
