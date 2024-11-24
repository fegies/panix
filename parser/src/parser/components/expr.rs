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
        let mut pos = t.position;
        let lhs = match t.token {
            Token::Ident(ident) => {
                if matches!(self.expect_peek()?, Token::At | Token::Colon) {
                    NixExprContent::Code(Code::Lambda(self.parse_lambda(ident)?))
                } else {
                    NixExprContent::Code(Code::ValueReference { ident })
                }
            }
            Token::KwIf => NixExprContent::Code(Code::IfExpr(self.parse_if()?)),
            Token::Float(f) => NixExprContent::BasicValue(BasicValue::Float(f)),
            Token::Integer(i) => NixExprContent::BasicValue(BasicValue::Int(i)),
            Token::CurlyOpen => self.parse_attrset_or_destructuring_lambda()?,
            Token::SquareOpen => {
                NixExprContent::CompoundValue(CompoundValue::List(self.parse_list()?))
            }
            Token::RoundOpen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RoundClose)?;
                pos = expr.position;
                expr.content
            }
            Token::KwLet => NixExprContent::Code(Code::LetInExpr(self.parse_let()?)),
            Token::KwWith => NixExprContent::Code(Code::WithExpr(self.parse_with_expr()?)),
            Token::KwRec => {
                NixExprContent::CompoundValue(CompoundValue::Attrset(self.parse_attrset_rec()?))
            }
            Token::KwNull => NixExprContent::BasicValue(BasicValue::Null),
            Token::StringBegin => {
                NixExprContent::BasicValue(BasicValue::String(self.parse_simple_string()?))
            }
            Token::IndentedStringBegin => {
                NixExprContent::BasicValue(BasicValue::String(self.parse_multiline_string()?))
            }
            Token::PathBegin => NixExprContent::BasicValue(BasicValue::Path(self.parse_path()?)),
            Token::Not | Token::Minus => {
                let (r_bp, opcode) = match t.token {
                    Token::Not => (RIGHT_BINDING_POWER_BOOL_NOT, MonopOpcode::BinaryNot),
                    Token::Minus => (RIGHT_BINDING_POWER_UNARY_MINUS, MonopOpcode::NumericMinus),
                    _ => unreachable!(),
                };
                let body = self.parse_with_bindingpower(r_bp, allow_spaces)?;
                NixExprContent::Code(Code::Op(Op::Monop {
                    opcode,
                    body: Box::new(body),
                }))
            }
            _ => unexpected(t)?,
        };
        let mut lhs = NixExpr {
            position: pos,
            content: lhs,
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

                let opcode = match token.token {
                    Token::Plus => Some(BinopOpcode::Add),
                    Token::Minus => Some(BinopOpcode::Subtract),
                    Token::Star => Some(BinopOpcode::Multiply),
                    Token::Slash => Some(BinopOpcode::Divide),
                    Token::DoublePlus => Some(BinopOpcode::ListConcat),
                    Token::DoubleSlash => Some(BinopOpcode::AttrsetMerge),
                    Token::Ne => Some(BinopOpcode::NotEqual),
                    Token::DoubleEq => Some(BinopOpcode::Equals),
                    Token::Or => Some(BinopOpcode::LogicalOr),
                    Token::And => Some(BinopOpcode::LogicalAnd),
                    Token::Le => Some(BinopOpcode::LessThanOrEqual),
                    Token::Lt => Some(BinopOpcode::LessThanStrict),
                    Token::Ge => Some(BinopOpcode::GreaterOrEqual),
                    Token::Gt => Some(BinopOpcode::GreaterThanStrict),
                    Token::Implication => Some(BinopOpcode::LogicalImplication),
                    _ => None,
                };

                let content = if let Some(opcode) = opcode {
                    let left = Box::new(lhs);
                    let right = Box::new(self.parse_with_bindingpower(r_bp, allow_spaces)?);
                    NixExprContent::Code(Code::Op(Op::Binop {
                        left,
                        right,
                        opcode,
                    }))
                } else {
                    match token.token {
                        Token::Dot => {
                            let left = Box::new(lhs);
                            // attrset refs are not full expressions. They may only be strings or idents
                            let name = self.parse_attrset_ref()?;
                            let default =
                                if let Some(Token::Ident("or")) = self.peek_no_whitespace() {
                                    // skip or keyword
                                    self.expect_next()?;
                                    Some(Box::new(self.parse_expr()?))
                                } else {
                                    None
                                };

                            NixExprContent::Code(Code::Op(Op::AttrRef {
                                left,
                                name,
                                default,
                            }))
                        }
                        Token::QuestionMark => NixExprContent::Code(Code::Op(Op::HasAttr {
                            left: Box::new(lhs),
                            path: self.parse_hasattr_path()?,
                        })),
                        Token::Whitespace => NixExprContent::Code(Code::Op(Op::Call {
                            function: Box::new(lhs),
                            arg: Box::new(self.parse_with_bindingpower(r_bp, allow_spaces)?),
                        })),
                        Token::RoundOpen => {
                            // this is a call, the arg is wrapped in braces
                            let expr = self.parse_expr()?;
                            self.expect(Token::RoundClose)?;
                            NixExprContent::Code(Code::Op(Op::Call {
                                function: Box::new(lhs),
                                arg: Box::new(expr),
                            }))
                        }
                        Token::SquareOpen => {
                            // this is a call, the arg is a list
                            let list = self.parse_list()?;
                            let list = NixExpr {
                                position: token.position,
                                content: NixExprContent::CompoundValue(CompoundValue::List(list)),
                            };

                            NixExprContent::Code(Code::Op(Op::Call {
                                function: Box::new(lhs),
                                arg: Box::new(list),
                            }))
                        }
                        Token::CurlyOpen => {
                            let attrset = NixExpr {
                                position: token.position,
                                content: NixExprContent::CompoundValue(CompoundValue::Attrset(
                                    self.parse_attrset_initial()?,
                                )),
                            };
                            NixExprContent::Code(Code::Op(Op::Call {
                                function: Box::new(lhs),
                                arg: Box::new(attrset),
                            }))
                        }
                        t => todo!("{:?}", t),
                    }
                };
                lhs = NixExpr {
                    content,
                    position: token.position,
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
            Token::Ident(ident) => Ok(NixString::from_literal(ident, t.position)),
            Token::StringBegin => self.parse_simple_string(),
            _ => unexpected(t),
        }
    }

    fn parse_hasattr_path(&mut self) -> ParseResult<AttrsetKey<'t>> {
        let first = self.parse_attrset_ref()?;

        if let Some(Token::Dot) = self.peek() {
            self.expect_next()?;
            let second = self.parse_attrset_ref()?;
            let mut segments = vec![first, second];
            while let Some(Token::Dot) = self.peek() {
                self.expect_next()?;
                segments.push(self.parse_attrset_ref()?);
            }
            Ok(AttrsetKey::Multi(segments))
        } else {
            Ok(AttrsetKey::Single(first))
        }
    }

    /// parse either a true attrset or a lambda with destructuring args
    /// without a total binding
    /// expects that the initial opening curly has already been consumed
    fn parse_attrset_or_destructuring_lambda(&mut self) -> ParseResult<NixExprContent<'t>> {
        let t = self.expect_next()?;
        match t.token {
            Token::Ident(first_ident) => {
                // we found an identifier. Now we need to figure out
                // if it is actually an attrset or a lambda.
                let t = self.expect_next()?;
                let res = match t.token {
                    Token::Dot => {
                        let attrset = self.parse_attrset_multipath(NixString::from_literal(
                            first_ident,
                            t.position,
                        ))?;
                        NixExprContent::CompoundValue(CompoundValue::Attrset(attrset))
                    }
                    Token::Eq => {
                        // this is a true attrset
                        let attrset =
                            self.parse_attrset(NixString::from_literal(first_ident, t.position))?;
                        NixExprContent::CompoundValue(CompoundValue::Attrset(attrset))
                    }
                    Token::QuestionMark => {
                        // this is a set of function args, the first one has a default arg
                        let default = self.parse_expr()?;
                        self.expect(Token::Comma)?;
                        let lambda = self.parse_attrset_lambda(first_ident, Some(default))?;
                        NixExprContent::Code(Code::Lambda(lambda))
                    }
                    Token::Comma => {
                        // this is a set of function args, no default
                        let lambda = self.parse_attrset_lambda(first_ident, None)?;
                        NixExprContent::Code(Code::Lambda(lambda))
                    }
                    Token::CurlyClose => {
                        // this must be a lambda with an attrset arg as { value } would be invalid
                        // for an attrset
                        self.expect(Token::Colon)?;
                        let body = self.parse_expr()?;

                        let mut bindings = HashMap::new();
                        bindings.insert(first_ident, None);

                        let lambda = Lambda {
                            args: LambdaArgs::AttrsetBinding {
                                total_name: None,
                                args: LambdaAttrsetArgs {
                                    bindings,
                                    includes_rest_pattern: false,
                                },
                            },
                            body: Box::new(body),
                        };

                        NixExprContent::Code(Code::Lambda(lambda))
                    }
                    _ => unexpected(t)?,
                };

                Ok(res)
            }
            Token::TripleDot => {
                // this must be a lambda, starting with a rest pattern.
                let lambda = self.parse_attrset_lambda_discard()?;
                Ok(NixExprContent::Code(Code::Lambda(lambda)))
            }
            Token::StringBegin => {
                // only attribute sets may have string keys.
                // so we know now it must be one.
                let initial_ident = self.parse_simple_string()?;
                let t = self.expect_next()?;
                let attrset = match t.token {
                    Token::Eq => self.parse_attrset(initial_ident)?,
                    Token::Dot => self.parse_attrset_multipath(initial_ident)?,
                    _ => unexpected(t)?,
                };
                Ok(NixExprContent::CompoundValue(CompoundValue::Attrset(
                    attrset,
                )))
            }
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
                    Ok(NixExprContent::Code(Code::Lambda(lambda)))
                } else {
                    // empty attrset
                    Ok(NixExprContent::CompoundValue(CompoundValue::Attrset(
                        Attrset::empty(),
                    )))
                }
            }
            Token::KwInherit => {
                // this is actually an attribute set starting with an inherit kw.
                let attrset = self.parse_attrset_inherit()?;
                Ok(NixExprContent::CompoundValue(CompoundValue::Attrset(
                    attrset,
                )))
            }
            _ => unexpected(t)?,
        }
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
            | Token::RoundOpen
            | Token::SquareOpen
    )
}

const RIGHT_BINDING_POWER_BOOL_NOT: u8 = 15;
const RIGHT_BINDING_POWER_UNARY_MINUS: u8 = 26;
fn infix_binding_power(token: &Token<'_>) -> Option<(u8, u8)> {
    // the binding power table.
    // higher-precedence operators have higher binding powers
    // right-assoc ops have the higher power left, left-assoc ones on the right.
    let res = match token {
        Token::Implication => (2, 1),
        Token::Or => (3, 4),
        Token::And => (5, 6),
        Token::Ne => (7, 8),
        Token::DoubleEq => (9, 10),
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

        // because of some stupid quirk of the interpreter, function calls can be
        // done without space if the next expr can be distinguished
        // This is the case with all types of braces
        Token::Whitespace | Token::RoundOpen | Token::CurlyOpen | Token::SquareOpen => (27, 28),
        Token::Dot => (29, 30),
        _ => return None,
    };
    Some(res)
}
