use lexer::Token;

use crate::parser::{ParseResult, TokenSource, ast::WithExpr};

use super::Parser;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse a with expression.
    /// Assumes the initial with keyword has already been consumed.
    pub fn parse_with_expr(&mut self) -> ParseResult<WithExpr<'t>> {
        let binding = self.parse_expr()?;
        self.expect(Token::Semicolon)?;
        let body = self.parse_expr()?;

        Ok(WithExpr {
            binding: Box::new(binding),
            body: Box::new(body),
        })
    }
}
