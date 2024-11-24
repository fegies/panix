use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse a inherit clause.
    /// Assumes an initial inherit keyword has already been consumed
    pub fn parse_inherit(&mut self) -> ParseResult<InheritEntry<'t>> {
        let mut entries = Vec::new();

        // either the first value (no source specified) or the source
        let t = self.expect_next()?;
        let source = match t.token {
            Token::Ident(varname) => {
                entries.push(varname);
                None
            }
            Token::RoundOpen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RoundClose)?;
                // we need to ensure that there is at least one ident provided.
                let t = self.expect_next()?;
                if let Token::Ident(varname) = t.token {
                    entries.push(varname);
                } else {
                    unexpected(t)?;
                }
                Some(Box::new(expr))
            }
            _ => unexpected(t)?,
        };

        // and collect the remaining values (if any)
        loop {
            let t = self.expect_next()?;
            match t.token {
                Token::Ident(varname) => {
                    entries.push(varname);
                }
                Token::Semicolon => {
                    break;
                }
                _ => {
                    unexpected(t)?;
                }
            }
        }

        Ok(InheritEntry { source, entries })
    }
}
