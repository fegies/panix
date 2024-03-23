use std::collections::HashMap;

use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse a set of destructuring arguments
    /// assumes the initial opening curly has been consumed
    fn parse_destructuring_args(
        &mut self,
        initial: Option<(&'t str, Option<NixExpr<'t>>)>,
    ) -> ParseResult<LambdaAttrsetArgs<'t>> {
        let mut includes_rest_pattern = false;
        let mut first = true;
        let mut bindings = HashMap::new();

        if let Some((ident, val)) = initial {
            bindings.insert(ident, val);
        }

        loop {
            if self.expect_peek()? == &Token::CurlyClose {
                self.expect_next()?;
                break;
            }

            if first {
                first = false;
            } else {
                self.expect(Token::Comma)?;
            }

            let t = self.expect_next()?;
            match t.token {
                Token::Ident(ident) => {
                    let body = if self.expect_peek()? == &Token::QuestionMark {
                        self.expect_next()?;
                        Some(self.parse_expr()?)
                    } else {
                        None
                    };
                    bindings.insert(ident, body);
                }
                Token::TripleDot => {
                    includes_rest_pattern = true;
                }
                _ => return unexpected(t),
            }
        }

        Ok(LambdaAttrsetArgs {
            bindings,
            includes_rest_pattern,
        })
    }

    /// parse a lambda expression. Assumes an initial ident has already been consumed.
    pub fn parse_lambda(&mut self, initial_ident: &'t str) -> ParseResult<Lambda<'t>> {
        let t = self.expect_next()?;
        match t.token {
            Token::At => {
                self.expect(Token::CurlyOpen)?;
                let args = self.parse_destructuring_args(None)?;
                self.expect(Token::Colon)?;
                let body = self.parse_expr()?;
                Ok(Lambda {
                    body: Box::new(body),
                    args: LambdaArgs::AttrsetBinding {
                        total_name: Some(initial_ident),
                        args,
                    },
                })
            }
            Token::Colon => {
                let body = self.parse_expr()?;
                Ok(Lambda {
                    args: LambdaArgs::SimpleBinding(initial_ident),
                    body: Box::new(body),
                })
            }
            _ => unexpected(t),
        }
    }

    pub fn parse_attrset_lambda(
        &mut self,
        initial_ident: &'t str,
        initial_value: Option<NixExpr<'t>>,
    ) -> ParseResult<Lambda<'t>> {
        let args = self.parse_destructuring_args(Some((initial_ident, initial_value)))?;
        self.expect(Token::Colon)?;
        let body = self.parse_expr()?;
        Ok(Lambda {
            args: LambdaArgs::AttrsetBinding {
                total_name: None,
                args,
            },
            body: Box::new(body),
        })
    }
}
