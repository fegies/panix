use std::collections::HashMap;

use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse a lambda expression. Assumes an initial ident has already been consumed.
    pub fn parse_lambda(&mut self, initial_ident: &'t str) -> ParseResult<Lambda<'t>> {
        let t = self.expect_next()?;
        match t.token {
            Token::At => {
                self.expect(Token::CurlyOpen)?;
                let args = self.parse_destructuring_args(None, false)?;
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

    /// parses a lambda with attrset args.
    /// assumes an opening curly and a discard pattern have already been consumed.
    pub fn parse_attrset_lambda_discard(&mut self) -> ParseResult<Lambda<'t>> {
        let args = self.parse_destructuring_args(None, true)?;
        self.parse_attrset_lambda_body(args)
    }

    /// Assumes that an opening curl, an initial identifier
    /// and an optional default value have already been parsed
    pub fn parse_attrset_lambda(
        &mut self,
        initial_ident: &'t str,
        initial_value: Option<NixExpr<'t>>,
    ) -> ParseResult<Lambda<'t>> {
        let args = self.parse_destructuring_args(Some((initial_ident, initial_value)), false)?;
        self.parse_attrset_lambda_body(args)
    }

    /// Optionally parses the total name of the attrset.
    /// Assumes that the argument def has already been parsed.
    fn parse_attrset_lambda_body(
        &mut self,
        args: LambdaAttrsetArgs<'t>,
    ) -> ParseResult<Lambda<'t>> {
        let t = self.expect_next()?;
        let total_name = match t.token {
            Token::At => {
                let ident = self.expect_ident()?;
                self.expect(Token::Colon)?;
                Some(ident)
            }
            Token::Colon => None,
            _ => unexpected(t)?,
        };

        let body = Box::new(self.parse_expr()?);
        Ok(Lambda {
            args: LambdaArgs::AttrsetBinding { total_name, args },
            body,
        })
    }

    /// parse a set of destructuring arguments
    /// assumes the initial opening curly has been consumed
    fn parse_destructuring_args(
        &mut self,
        initial: Option<(&'t str, Option<NixExpr<'t>>)>,
        mut includes_rest_pattern: bool,
    ) -> ParseResult<LambdaAttrsetArgs<'t>> {
        let mut first = !includes_rest_pattern;
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
                Token::TripleDot if !includes_rest_pattern => {
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
}
