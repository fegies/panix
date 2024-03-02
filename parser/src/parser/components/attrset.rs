use std::collections::HashMap;

use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse an attribute set.
    /// Assumes the initial opening brace and equality sign have already been consumed
    pub fn parse_attrset(&mut self, initial_ident: &'t str) -> ParseResult<Attrset<'t>> {
        let initial_value = self.parse_expr()?;
        self.expect(Token::Semicolon)?;

        let mut attrs = HashMap::new();
        attrs.insert(initial_ident, initial_value);

        loop {
            if let Token::CurlyClose = self.expect_peek()? {
                self.expect_next()?;
                break;
            }

            let ident = match self.expect_next()? {
                Token::Ident(i) => i,
                t => unexpected(t)?,
            };
            self.expect(Token::Eq)?;
            let value = self.parse_expr()?;
            self.expect(Token::Semicolon)?;

            attrs.insert(ident, value);
        }

        Ok(Attrset {
            is_recursive: false,
            attrs,
        })
    }
}
