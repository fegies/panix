use std::collections::HashMap;

use lexer::Token;

use crate::parser::{
    ast::{LetInExpr, NixExpr},
    ParseResult, TokenSource,
};

use super::{unexpected, Parser};

impl<'t, S: TokenSource<'t>> Parser<S> {
    fn parse_binding(&mut self) -> ParseResult<(&'t str, NixExpr<'t>)> {
        let t = self.expect_next()?;
        let ident = match t.token {
            Token::Ident(ident) => ident,
            _ => return unexpected(t),
        };

        self.expect(Token::Eq)?;

        let body = self.parse_expr()?;
        self.expect(Token::Semicolon)?;

        Ok((ident, body))
    }

    /// parse a let in expression.
    /// assumes that the initial let has already been parsed
    pub fn parse_let(&mut self) -> ParseResult<LetInExpr<'t>> {
        let mut bindings = HashMap::new();

        while let Token::Ident(_) = self.expect_peek()? {
            let (ident, body) = self.parse_binding()?;
            bindings.insert(ident, body);
        }

        self.expect(Token::KwIn)?;

        let body = Box::new(self.parse_expr()?);

        Ok(LetInExpr { bindings, body })
    }
}
