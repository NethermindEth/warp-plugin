use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::ast::{
    Expr, ExprFunctionCall, FunctionWithBody, ModuleBody, PathSegment,
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
        if let ast::OptionAttributeArgs::AttributeArgs(args) = custom_implicts.args(db) {
            let mut implicits = vec![];
            let mut diagnostics = vec![];

            let attr_args = args.arg_list(db).elements(db);
            if attr_args.len() == 0 {
                return Ok(None);
            }
            if attr_args.len() % 2 != 0 {
                return Err(vec![PluginDiagnostic {
                    stable_ptr: custom_implicts.stable_ptr().untyped(),
                    message: "Invalid amount of parameters".into(),
                }]);
            }

            for i in (0..attr_args.len()).step_by(2) {
                let name_expr = &attr_args[i];
                let type_expr = &attr_args[i + 1];

                if let [Expr::Path(path_name), Expr::Path(path_type)] = [name_expr, type_expr] {
                    if let [[PathSegment::Simple(segment_name)], [PathSegment::Simple(segment_type)]] =
                        [&path_name.elements(db)[..], &path_type.elements(db)[..]]
                    {
                        let name = segment_name.ident(db).text(db);
                        let typex = segment_type.ident(db).text(db);
                        implicits.push(ImplicitInfo { name, typex });
                    } else {
                        diagnostics.push(PluginDiagnostic {
                            stable_ptr: path_name.stable_ptr().untyped(),
                            message: "Expected two path segments.".into(),
                        });
                    }
                } else {
                    dbg!(name_expr.as_syntax_node().kind(db).get_text());
                    dbg!(type_expr.as_syntax_node().kind(db).get_text());
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: name_expr.stable_ptr().untyped(),
                        message: "Expected path expressions.".into(),
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

pub fn get_func_call_name(db: &dyn SyntaxGroup, func_call: &ExprFunctionCall) -> SmolStr {
    let path = func_call.path(db);
    if let [ast::PathSegment::Simple(func_name_token)] = &path.elements(db)[..] {
        return func_name_token.ident(db).text(db);
    }
    if let [ast::PathSegment::Simple(_), ast::PathSegment::Simple(b)] = &path.elements(db)[..] {
        // dbg!("Here {0} {1}", a.ident(db).text(db), b.ident(db).text(db));
        return b.ident(db).text(db);
    }
    // let count = path.elements(db).len();
    // dbg!(count);
    panic!("Couldn't get func call name");
}

pub trait IsExpression {
    fn is_expression(&self) -> bool;

    // todo: delete after debugging
    fn get_text(&self) -> String;
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
    fn get_text(&self) -> String {
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
