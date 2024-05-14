use std::collections::HashMap;

use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    pub fn parse_expr(&mut self) -> ParseResult<NixExpr<'t>> {
        self.parse_with_bindingpower(0, true)
    }
    pub fn parse_expr_no_spaces(&mut self) -> ParseResult<NixExpr<'t>> {
        self.parse_with_bindingpower(0, false)
    }

    fn parse_with_bindingpower(
        &mut self,
        min_bp: u8,
        allow_spaces: bool,
    ) -> ParseResult<NixExpr<'t>> {
        let t = self.expect_next()?;
        let mut lhs = match t.token {
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
            Token::KwWith => NixExpr::Code(Code::WithExpr(self.parse_with_expr()?)),
            Token::KwRec => {
                NixExpr::CompoundValue(CompoundValue::Attrset(self.parse_attrset_rec()?))
            }
            Token::KwNull => NixExpr::BasicValue(BasicValue::Null),
            Token::StringBegin => {
                NixExpr::BasicValue(BasicValue::String(self.parse_simple_string()?))
            }
            Token::IndentedStringBegin => {
                NixExpr::BasicValue(BasicValue::String(self.parse_multiline_string()?))
            }
            Token::PathBegin => NixExpr::BasicValue(BasicValue::Path(self.parse_path()?)),
            Token::Not => todo!(),
            _ => unexpected(t)?,
        };

        loop {
            if let Some((l_bp, r_bp)) = self.peek().and_then(infix_binding_power) {
                if l_bp < min_bp {
                    break;
                }

                let token = self.expect_next_or_whitespace()?;

                if token.token == Token::Whitespace {
                    if !allow_spaces {
                        break;
                    }
                    if !could_start_expression(self.expect_peek()?) {
                        continue;
                    }
                }

                lhs = match token.token {
                    Token::Dot => NixExpr::Code(Code::Op(Op::AttrRef {
                        left: Box::new(lhs),
                        // attrset refs are not full expressions. They may only be strings or idents
                        name: self.parse_attrset_ref()?,
                    })),
                    Token::Whitespace => NixExpr::Code(Code::Op(Op::Call {
                        function: Box::new(lhs),
                        arg: Box::new(self.parse_with_bindingpower(r_bp, allow_spaces)?),
                    })),
                    t @ (Token::Plus | Token::DoublePlus | Token::DoubleSlash) => {
                        let opcode = match t {
                            Token::Plus => BinopOpcode::Add,
                            Token::DoublePlus => BinopOpcode::ListConcat,
                            Token::DoubleSlash => BinopOpcode::AttrsetMerge,
                            _ => unreachable!(),
                        };
                        let left = Box::new(lhs);
                        let right = Box::new(self.parse_with_bindingpower(r_bp, allow_spaces)?);
                        NixExpr::Code(Code::Op(Op::Binop {
                            left,
                            right,
                            opcode,
                        }))
                    }
                    t => todo!("{:?}", t),
                }
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_attrset_ref(&mut self) -> ParseResult<NixString<'t>> {
        let t = self.expect_next()?;
        match t.token {
            Token::Ident(ident) => Ok(NixString::Literal(ident)),
            Token::StringBegin => self.parse_simple_string(),
            _ => unexpected(t),
        }
    }

    /// parse either a true attrset or a lambda with destructuring args
    /// without a total binding
    /// expects that the initial opening curly has already been consumed
    fn parse_attrset_or_destructuring_lambda(&mut self) -> ParseResult<NixExpr<'t>> {
        let t = self.expect_next()?;
        let first_ident = match t.token {
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
                return Ok(NixExpr::CompoundValue(CompoundValue::Attrset(
                    Attrset::empty(),
                )));
            }
            _ => unexpected(t)?,
        };

        let t = self.expect_next()?;
        let res = match t.token {
            Token::Dot => {
                let attrset = self.parse_attrset_multipath(first_ident)?;
                NixExpr::CompoundValue(CompoundValue::Attrset(attrset))
            }
            Token::Eq => {
                // this is a true attrset
                let attrset = self.parse_attrset(first_ident)?;
                NixExpr::CompoundValue(CompoundValue::Attrset(attrset))
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
                let lambda = self.parse_attrset_lambda(first_ident, None)?;
                NixExpr::Code(Code::Lambda(lambda))
            }
            _ => unexpected(t)?,
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
            | Token::IndentedStringBegin
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
