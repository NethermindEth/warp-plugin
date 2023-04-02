use std::collections::HashMap;

use cairo_lang_defs::plugin::{PluginDiagnostic, PluginResult};
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_syntax::node::{
    ast::{Expr, FunctionWithBody, ModuleBody, Statement},
    db::SyntaxGroup,
    TypedSyntaxNode,
};
use itertools::Itertools;

const STATEMENT: String = String::from("statement");

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
    let mut diagnostics: Vec<PluginDiagnostic> = vec![];
    let mut rewrite_body: HashMap<String, RewriteNode> = HashMap::new();

    // Handle the expression block
    // TODO: We need to add initialization statments for each implicits in case of being necesary
    let mut index = 0;
    for statement in function_body.body(db).statements(db).elements(db) {
        let (rewrite_statement, statement_diagnostics) =
            handle_statement(db, function_with_implicits, statement);

        if statement_diagnostics.len() > 0 {
            diagnostics.extend(statement_diagnostics);
        }

        let statement_key = format!("{STATEMENT}_{index}");
        rewrite_body.insert(statement_key, rewrite_statement);
        index += 1;
    }
    // Here we rewrite the expression block;
    let expr_block_str = (0..index).map(|i| format!("${STATEMENT}_{i}$")).join("\n");
    let expr_bloc_rewrriten = RewriteNode::interpolate_patched(&expr_block_str, rewrite_body);

    // Handle the declaration (similar to handle implicit)

    todo!();
}

fn handle_statement(
    db: &dyn SyntaxGroup,
    function_with_implicits: &HashMap<FuncName, Implicit>,
    statement: Statement,
) -> HandlingResult {
    todo!();
}

fn handle_expression(
    db: &dyn SyntaxGroup,
    function_with_implicits: &HashMap<FuncName, Implicit>,
    expression: Expr,
) -> HandlingResult {
    todo!();
}
