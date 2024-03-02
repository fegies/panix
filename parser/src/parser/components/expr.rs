use std::collections::HashMap;

use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    pub fn parse_expr(&mut self) -> ParseResult<NixExpr<'t>> {
        self.parse_with_bindingpower(0)
    }

    fn parse_with_bindingpower(&mut self, min_bp: u8) -> ParseResult<NixExpr<'t>> {
        let mut lhs = match self.expect_next()? {
            Token::Ident(ident) => {
                if matches!(self.expect_peek()?, Token::At | Token::Colon) {
                    NixExpr::Code(Code::Lambda(self.parse_lambda(ident)?))
                } else {
                    NixExpr::Code(Code::ValueReference { ident })
                }
            }
            Token::KwIf => NixExpr::Code(Code::IfExpr(self.parse_if()?)),
            Token::Float(f) => NixExpr::BasicValue(BasicValue::Float(f)),
            Token::Integer(i) => NixExpr::BasicValue(BasicValue::Int(i)),
            Token::CurlyOpen => self.parse_attrset_or_destructuring_lambda()?,
            Token::SquareOpen => NixExpr::CompoundValue(CompoundValue::List(self.parse_list()?)),
            Token::Minus => todo!(),
            Token::RoundOpen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RoundClose)?;
                expr
            }
            Token::KwLet => NixExpr::Code(Code::LetInExpr(self.parse_let()?)),
            Token::KwWith => todo!(),
            Token::KwRec => todo!(),
            Token::KwNull => NixExpr::BasicValue(BasicValue::Null),
            Token::StringBegin => {
                NixExpr::BasicValue(BasicValue::String(self.parse_simple_string()?))
            }
            Token::IndentedStringBegin => {
                NixExpr::BasicValue(BasicValue::String(self.parse_multiline_string()?))
            }
            Token::PathBegin => todo!(),
            Token::Not => todo!(),
            t => unexpected(t)?,
        };

        loop {
            println!("lhs: {lhs:?}");
            if let Some((l_bp, r_bp)) = self.peek().and_then(infix_binding_power) {
                if l_bp < min_bp {
                    break;
                }

                let token = self.expect_next_or_whitespace()?;

                if token == Token::Whitespace && !could_start_expression(self.expect_peek()?) {
                    continue;
                }

                lhs = match token {
                    Token::Dot => NixExpr::Code(Code::Op(Op::AttrRef {
                        left: Box::new(lhs),
                        // attrset refs are not full expressions. They may only be strings or idents
                        name: self.parse_attrset_ref()?,
                    })),
                    Token::Whitespace => NixExpr::Code(Code::Op(Op::Call {
                        function: Box::new(lhs),
                        arg: Box::new(self.parse_with_bindingpower(r_bp)?),
                    })),
                    Token::Plus => NixExpr::Code(Code::Op(Op::Add {
                        left: Box::new(lhs),
                        right: Box::new(self.parse_with_bindingpower(r_bp)?),
                    })),
                    t => todo!("{:?}", t),
                }
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_attrset_ref(&mut self) -> ParseResult<&'t str> {
        match self.expect_next()? {
            Token::Ident(ident) => Ok(ident),
            t => unexpected(t),
        }
    }

    /// parse either a true attrset or a lambda with destructuring args
    /// without a total binding
    /// expects that the initial opening curly has already been consumed
    fn parse_attrset_or_destructuring_lambda(&mut self) -> ParseResult<NixExpr<'t>> {
        let first_ident = match self.expect_next()? {
            Token::Ident(i) => i,
            Token::CurlyClose => {
                if let Token::Colon = self.expect_peek()? {
                    // this is actually a lambda with an empty args list
                    self.expect_next()?;
                    let body = self.parse_expr()?;
                    let lambda = Lambda {
                        args: LambdaArgs::AttrsetBinding {
                            total_name: None,
                            args: LambdaAttrsetArgs {
                                bindings: HashMap::new(),
                                includes_rest_pattern: false,
                            },
                        },
                        body: Box::new(body),
                    };
                    return Ok(NixExpr::Code(Code::Lambda(lambda)));
                }
                // empty attrset
                return Ok(NixExpr::CompoundValue(CompoundValue::Attrset(Attrset {
                    is_recursive: false,
                    attrs: HashMap::new(),
                })));
            }
            t => unexpected(t)?,
        };

        let res = match self.expect_next()? {
            Token::Eq => {
                // this is a true attrset
                todo!()
            }
            Token::QuestionMark => {
                // this is a set of function args, the first one has a default arg
                let default = self.parse_expr()?;
                self.expect(Token::Comma)?;
                let lambda = self.parse_attrset_lambda(first_ident, Some(default))?;
                NixExpr::Code(Code::Lambda(lambda))
            }
            Token::Comma => {
                // this is a set of function args, no default
                todo!()
            }
            t => unexpected(t)?,
        };

        Ok(res)
    }
}

fn could_start_expression(token: &Token) -> bool {
    matches!(
        token,
        Token::Ident(_)
            | Token::CurlyOpen
            | Token::PathBegin
            | Token::StringBegin
            | Token::Float(_)
            | Token::Integer(_)
            | Token::KwIn
            | Token::KwLet
            | Token::KwRec
            | Token::KwWith
            | Token::Minus
            | Token::RoundOpen
            | Token::SquareOpen
    )
}

fn infix_binding_power(token: &Token<'_>) -> Option<(u8, u8)> {
    println!("getting infix for {token:?}");
    // the binding power table.
    // higher-precedence operators have higher binding powers
    // right-assoc ops have the higher power left, left-assoc ones on the right.
    let res = match token {
        Token::Implication => (2, 1),
        Token::Or => (3, 4),
        Token::And => (5, 6),
        Token::Ne => (7, 8),
        Token::Eq => (9, 10),
        Token::Ge => (11, 12),
        Token::Gt => (11, 12),
        Token::Le => (11, 12),
        Token::Lt => (11, 12),
        Token::DoubleSlash => (14, 13),
        // this is where the bool not would go, with bp 15
        Token::Plus => (16, 17),
        Token::Minus => (16, 17),
        Token::Slash => (18, 19),
        Token::Star => (20, 21),
        Token::DoublePlus => (23, 22),
        Token::QuestionMark => (24, 25),
        // this is where the unary minus would go, with bp 26
        Token::Whitespace => (27, 28),
        Token::Dot => (29, 30),
        _ => return None,
    };
    Some(res)
}
