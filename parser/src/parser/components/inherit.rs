use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse a inherit clause.
    /// Assumes an initial inherit keyword has already been consumed
    pub fn parse_inherit(&mut self) -> ParseResult<InheritEntry<'t>> {
        let source = if self.expect_peek()? == &Token::RoundOpen {
            // we have a source.
            self.expect(Token::RoundOpen)?;
            let expr = Box::new(self.parse_expr()?);
            self.expect(Token::RoundClose)?;
            Some(expr)
        } else {
            None
        };

        let mut entries = Vec::new();
        // and collect up all the entries.
        loop {
            let t = self.expect_next()?;
            match t.token {
                Token::Ident(varname) => {
                    entries.push(varname);
                }
                Token::StringBegin => {
                    let ident_value = self.parse_simple_string(t.position)?;
                    if let Some(ident) = ident_value.content.get_literal() {
                        entries.push(ident);
                    } else {
                        return Err(ParseError::UnexpectedToken(
                            "Only simple strings allowed".to_owned(),
                        ));
                    }
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
