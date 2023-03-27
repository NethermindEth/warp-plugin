use std::vec;

use cairo_lang_syntax::node::ast::{self, Expr, ExprBlock, ExprFunctionCall, Statement};
use cairo_lang_syntax::node::db::SyntaxGroup;

pub fn handle_function_calls(
    db: &dyn SyntaxGroup,
    function_ast: &ast::FunctionWithBody,
) -> Vec<ExprFunctionCall> {
    let expressions = extract_expressions_from_expr_block(db, function_ast.body(db));
    let func_calls: Vec<ExprFunctionCall> = expressions
        .into_iter()
        .filter_map(|expr| match expr {
            Expr::FunctionCall(expr) => Some(expr),
            _ => None,
        })
        .collect();
    return func_calls;
}

fn extract_expressions_from_expr_block(db: &dyn SyntaxGroup, expr_block: ExprBlock) -> Vec<Expr> {
    expr_block
        .statements(db)
        .elements(db)
        .into_iter()
        .map(|statement| extract_expressions_from_statements(db, statement))
        .flatten()
        .collect()
}

fn extract_expressions_from_statements(db: &dyn SyntaxGroup, statement: Statement) -> Vec<Expr> {
    match statement {
        Statement::Expr(statement) => extract_expressions_from_expression(db, statement.expr(db)),
        Statement::Let(statement) => extract_expressions_from_expression(db, statement.rhs(db)),
        Statement::Return(statement) => extract_expressions_from_expression(db, statement.expr(db)),
        Statement::Missing(_) => vec![],
    }
}

fn extract_expressions_from_expression(db: &dyn SyntaxGroup, expression: Expr) -> Vec<Expr> {
    let mut all_expr = vec![expression.clone()];
    let nested_exprs = match expression {
        Expr::Block(expr) => vec![extract_expressions_from_expr_block(db, expr)],
        Expr::Binary(expr) => vec![
            extract_expressions_from_expression(db, expr.lhs(db)),
            extract_expressions_from_expression(db, expr.rhs(db)),
        ],
        Expr::ErrorPropagate(expr) => vec![extract_expressions_from_expression(db, expr.expr(db))],
        Expr::FunctionCall(expr) => {
            // TODO: Arguments appear not to be expressions
            vec![]
        }
        Expr::If(expr) => vec![
            extract_expressions_from_expression(db, expr.condition(db)),
            extract_expressions_from_expr_block(db, expr.if_block(db)),
        ],
        Expr::Indexed(expr) => vec![extract_expressions_from_expression(db, expr.expr(db))],
        Expr::Match(expr) => {
            let arm_exprs: Vec<Expr> = expr
                .arms(db)
                .elements(db)
                .iter()
                .flat_map(|arm| extract_expressions_from_expression(db, arm.expression(db)))
                .collect();
            vec![
                extract_expressions_from_expression(db, expr.expr(db)),
                arm_exprs,
            ]
        }
        Expr::StructCtorCall(expr) => {
            // TODO: Arguments appear not to be expressions
            // expr.arguments(db)
            //     .arguments(db)
            //     .elements(db)
            //     .into_iter()
            //     .map(|arg| arg);
            vec![]
        }

        Expr::Tuple(expr) => {
            let sub_expressions: Vec<Expr> = expr
                .expressions(db)
                .elements(db)
                .into_iter()
                .flat_map(|sub_expr| extract_expressions_from_expression(db, sub_expr))
                .collect();
            vec![sub_expressions]
        }
        _ => vec![],
    };

    let flat_nested_expr = nested_exprs.into_iter().flatten();
    all_expr.extend(flat_nested_expr);
    all_expr
}
