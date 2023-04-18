use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::ast::{
    ArgClause, Expr, ExprFunctionCall, FunctionWithBody, ModuleBody, PathSegment,
};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, db::SyntaxGroup, Terminal, TypedSyntaxNode};
use smol_str::SmolStr;
use std::collections::HashMap;
use std::string::String;

const IMPLICIT_ATTR: &str = "implicit";

pub type FuncName = SmolStr;
pub type Implicits = Vec<SmolStr>;

#[derive(Debug)]
pub struct ImplicitInfo {
    pub name: SmolStr,
    pub typex: SmolStr,
}

pub fn gather_function_with_implicits(
    db: &dyn SyntaxGroup,
    module_body: &ModuleBody,
) -> HashMap<FuncName, Implicits> {
    module_body.items(db).elements(db).into_iter().fold(
        HashMap::new(),
        |mut name_to_implicit, item| match item {
            ast::Item::FreeFunction(func) => {
                if let Ok(Some(implicits)) = extract_implicit_attributes(db, &func) {
                    let func_name = func.declaration(db).name(db).text(db);
                    name_to_implicit
                        .insert(func_name, implicits.into_iter().map(|ii| ii.name).collect());
                }
                name_to_implicit
            }
            _ => name_to_implicit,
        },
    )
}

pub fn extract_implicit_attributes(
    db: &dyn SyntaxGroup,
    func: &FunctionWithBody,
) -> Result<Option<Vec<ImplicitInfo>>, Vec<PluginDiagnostic>> {
    let maybe_custom_implicit = func
        .attributes(db)
        .elements(db)
        .into_iter()
        .find(|attr| attr.attr(db).text(db) == IMPLICIT_ATTR);

    if let Some(custom_implicts) = maybe_custom_implicit {
        if let ast::OptionArgListParenthesized::ArgListParenthesized(arg_list) =
            custom_implicts.arguments(db)
        {
            let mut implicits = vec![];
            let mut diagnostics = vec![];

            let attr_args = arg_list.args(db).elements(db);
            if attr_args.len() == 0 {
                return Ok(None);
            }

            for arg in attr_args {
                if let ArgClause::Named(named_arg) = arg.arg_clause(db) {
                    let name_text = named_arg.name(db).text(db);
                    let typex = named_arg.value(db);

                    let simple_path_text = simple_path_from_expr(db, typex);
                    if let Ok(type_text) = simple_path_text {
                        implicits.push(ImplicitInfo {
                            name: name_text,
                            typex: type_text,
                        })
                    } else {
                        let error = simple_path_text.err().unwrap();
                        diagnostics.push(error);
                    }
                } else {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: arg.stable_ptr().untyped(),
                        message: "Expected an argument in the form of: <name>: <type>.".into(),
                    });
                }
            }
            return if diagnostics.len() == 0 {
                Ok(Some(implicits))
            } else {
                Err(diagnostics)
            };
        }
    }
    Ok(None)
}

pub fn get_implicit_arg(db: &dyn SyntaxGroup, arg: Expr) -> Option<SmolStr> {
    if let ast::Expr::Path(expr) = arg {
        if let [ast::PathSegment::Simple(segment)] = &expr.elements(db)[..] {
            return Some(segment.ident(db).text(db));
        }
    }
    None
}

pub fn get_func_call_name(db: &dyn SyntaxGroup, func_call: &ExprFunctionCall) -> Option<SmolStr> {
    let path = func_call.path(db);
    if let Some(ast::PathSegment::Simple(func_name_token)) = &path.elements(db)[..].last() {
        return Some(func_name_token.ident(db).text(db));
    }
    None
    //let function_call_name = path
    //    .elements(db)
    //    .into_iter()
    //    .map(|p| p.as_syntax_node().get_text(db))
    //    .join("::");
    //panic!(
    //    "Couldn not read function call name: {0}",
    //    function_call_name
    //);
}

fn simple_path_from_expr(db: &dyn SyntaxGroup, expr: Expr) -> Result<SmolStr, PluginDiagnostic> {
    let Expr::Path(path) = expr else {
        println!("Is not path expr, it is: {0}", expr.as_syntax_node().kind(db).get_expr_type());
        return  Err(PluginDiagnostic{
            stable_ptr: expr.stable_ptr().untyped(), message: "Expected a path expression for type parsing".into()
        });
    };
    let [PathSegment::Simple(segment)] = &path.elements(db)[..] else {
        return  Err(PluginDiagnostic{
            stable_ptr: path.stable_ptr().untyped(), message: "Expected a Simple Path when parsing argument type".into()
        });
    };
    Ok(segment.ident(db).text(db))
}

pub trait IsExpression {
    fn is_expression(&self) -> bool;

    fn get_expr_type(&self) -> String;
}

impl IsExpression for SyntaxKind {
    fn is_expression(&self) -> bool {
        matches!(
            *self,
            SyntaxKind::ExprPath
                | SyntaxKind::TerminalLiteralNumber
                | SyntaxKind::TerminalShortString
                | SyntaxKind::TerminalFalse
                | SyntaxKind::TerminalTrue
                | SyntaxKind::ExprParenthesized
                | SyntaxKind::ExprUnary
                | SyntaxKind::ExprBinary
                | SyntaxKind::ExprTuple
                | SyntaxKind::ExprFunctionCall
                | SyntaxKind::ExprStructCtorCall
                | SyntaxKind::ExprBlock
                | SyntaxKind::ExprMatch
                | SyntaxKind::ExprIf
                | SyntaxKind::ExprErrorPropagate
                | SyntaxKind::ExprFieldInitShorthand
                | SyntaxKind::ExprIndexed
                | SyntaxKind::ExprMissing
        )
    }

    // todo: delete after debugging
    fn get_expr_type(&self) -> String {
        let branch_name = match self {
            SyntaxKind::ExprPath => "ExprPath",
            SyntaxKind::TerminalLiteralNumber => "TerminalLiteralNumber",
            SyntaxKind::TerminalShortString => "TerminalShortString",
            SyntaxKind::TerminalFalse => "TerminalFalse",
            SyntaxKind::TerminalTrue => "TerminalTrue",
            SyntaxKind::ExprParenthesized => "ExprParenthesized",
            SyntaxKind::ExprUnary => "ExprUnary",
            SyntaxKind::ExprBinary => "ExprBinary",
            SyntaxKind::ExprTuple => "ExprTuple",
            SyntaxKind::ExprFunctionCall => "ExprFunctionCall",
            SyntaxKind::ExprStructCtorCall => "ExprStructCtorCall",
            SyntaxKind::ExprBlock => "ExprBlock",
            SyntaxKind::ExprMatch => "ExprMatch",
            SyntaxKind::ExprIf => "ExprIf",
            SyntaxKind::ExprErrorPropagate => "ExprErrorPropagate",
            SyntaxKind::ExprFieldInitShorthand => "ExprFieldInitShorthand",
            SyntaxKind::ExprIndexed => "ExprIndexed",
            SyntaxKind::ExprMissing => "ExprMissing",
            _ => panic!("Unexpected SyntaxKind: {:?}", self),
        };
        String::from(branch_name)
    }
}
