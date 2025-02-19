use std::collections::BTreeMap;

use lexer::Token;

use crate::parser::{ast::LetInExpr, ParseResult, TokenSource};

use super::{unexpected, LetExpr, Parser};

impl<'t, S: TokenSource<'t>> Parser<S> {
    pub fn parse_let(&mut self) -> ParseResult<LetExpr<'t>> {
        if let Token::CurlyOpen = self.expect_peek()? {
            self.expect_next()?;
            Ok(LetExpr::AttrsetLet(self.parse_attrset_initial()?))
        } else {
            Ok(LetExpr::LetIn(self.parse_let_in()?))
        }
    }

    /// parse a let in expression.
    /// assumes that the initial let has already been parsed
    fn parse_let_in(&mut self) -> ParseResult<LetInExpr<'t>> {
        let mut bindings = BTreeMap::new();
        let mut inherit_entries = Vec::new();

        loop {
            let t = self.expect_next()?;
            match t.token {
                Token::KwInherit => {
                    inherit_entries.push(self.parse_inherit()?);
                }
                Token::Ident(ident) => {
                    self.expect(Token::Eq)?;
                    let body = self.parse_expr()?;
                    bindings.insert(ident, body);
                    self.expect(Token::Semicolon)?;
                }
                Token::KwIn => {
                    break;
                }
                _ => unexpected(t)?,
            }
        }

        let body = Box::new(self.parse_expr()?);

        Ok(LetInExpr {
            bindings,
            inherit_entries,
            body,
        })
    }
}
