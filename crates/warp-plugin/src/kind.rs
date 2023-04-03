use cairo_lang_syntax::node::kind::SyntaxKind;

pub trait IsExpression {
    fn is_expression(&self) -> bool;
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
}
