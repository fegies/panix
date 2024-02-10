use lexer::Token;

use super::{ParseResult, TokenSource};

use super::ast::*;

pub struct Parser<S> {
    source: S,
}

fn unexpected<T>(t: Token) -> ParseResult<T> {
    return Err(super::ParseError::UnexpectedToken(t));
}

impl<'t, S> Parser<S>
where
    S: TokenSource<'t>,
{
    pub fn new(source: S) -> Self {
        Self { source }
    }

    pub fn run(mut self) -> ParseResult<'t, NixExpr<'t>> {
        let res = self.parse_expr()?;
        self.expect(Token::EOF)?;
        Ok(res)
    }

    fn parse_expr(&mut self) -> ParseResult<'t, NixExpr<'t>> {
        match self.expect_next()? {
            Token::StringBegin => self
                .parse_simple_string()
                .map(BasicValue::String)
                .map(NixExpr::BasicValue),
            Token::Ident(ident) => {
                println!("found {ident:?}");
                todo!()
            }
            Token::At => todo!(),
            Token::CurlyOpen => todo!(),
            Token::CurlyClose => todo!(),
            Token::SquareOpen => todo!(),
            Token::SquareClose => todo!(),
            Token::Dot => todo!(),
            Token::TripleDot => todo!(),
            Token::Slash => todo!(),
            Token::DoubleSlash => todo!(),
            Token::Eq => todo!(),
            Token::DoubleEq => todo!(),
            Token::Colon => todo!(),
            Token::Semicolon => todo!(),
            Token::Plus => todo!(),
            Token::DoublePlus => todo!(),
            Token::Minus => todo!(),
            Token::Star => todo!(),
            Token::RoundOpen => todo!(),
            Token::RoundClose => todo!(),
            Token::KwLet => todo!(),
            Token::KwIn => todo!(),
            Token::KwWith => todo!(),
            Token::KwRec => todo!(),
            Token::KwNull => todo!(),
            Token::Comma => todo!(),
            Token::StringEnd => todo!(),
            Token::IndentedStringBegin => todo!(),
            Token::PathBegin => todo!(),
            Token::PathEnd => todo!(),
            Token::StringContent(_) => todo!(),
            Token::BeginInterpol => todo!(),
            Token::EndInterpol => todo!(),
            Token::Implication => todo!(),
            Token::Lt => todo!(),
            Token::Le => todo!(),
            Token::Gt => todo!(),
            Token::Ge => todo!(),
            Token::Not => todo!(),
            Token::Ne => todo!(),
            Token::Or => todo!(),
            Token::QuestionMark => todo!(),
            Token::EOF => todo!(),
        }
    }

    fn expect_next(&mut self) -> ParseResult<'t, Token<'t>> {
        self.source.next().ok_or(super::ParseError::UnexpectedEof)
    }
    fn expect_peek(&mut self) -> ParseResult<'t, &Token<'t>> {
        self.source.peek().ok_or(super::ParseError::UnexpectedEof)
    }

    fn expect(&mut self, token: Token<'static>) -> ParseResult<'t, ()> {
        let next = self.expect_next()?;
        if next == token {
            Ok(())
        } else {
            Err(super::ParseError::UnexpectedToken(next))
        }
    }

    /// parse an interpolation.
    /// This method assumes that the beginInterpolation token has already been consumed.
    fn parse_interpolated_string(
        &mut self,
        initial_part: NixString<'t>,
    ) -> ParseResult<'t, NixString<'t>> {
        let mut parts = match initial_part {
            NixString::Literal(lit) => vec![InterpolationEntry::LiteralPiece(lit)],
            NixString::Composite(p) => p
                .into_iter()
                .map(InterpolationEntry::LiteralPiece)
                .collect(),
            NixString::Interpolated(p) => p,
            NixString::Empty => Vec::new(),
        };

        let expression = self.parse_expr()?;
        parts.push(InterpolationEntry::Expression(expression));
        self.expect(Token::EndInterpol)?;
        loop {
            match self.expect_next()? {
                Token::StringContent(str) => {
                    parts.push(InterpolationEntry::LiteralPiece(str));
                }
                Token::BeginInterpol => {
                    let expr = self.parse_expr()?;
                    parts.push(InterpolationEntry::Expression(expr));
                    self.expect(Token::EndInterpol)?;
                }
                Token::StringEnd => {
                    return Ok(NixString::Interpolated(parts));
                }
                t => return unexpected(t),
            }
        }
    }

    fn parse_simple_string(&mut self) -> ParseResult<'t, NixString<'t>> {
        self.parse_string_content()
    }

    fn parse_string_content(&mut self) -> ParseResult<'t, NixString<'t>> {
        let initial_content = match self.expect_next()? {
            Token::StringEnd => return Ok(NixString::Empty),
            Token::StringContent(cont) => cont,
            Token::BeginInterpol => {
                return self.parse_interpolated_string(NixString::Empty);
            }
            t => return unexpected(t),
        };
        let next_part = match self.expect_next()? {
            Token::StringEnd => return Ok(NixString::Literal(initial_content)),
            Token::StringContent(cont) => cont,
            Token::BeginInterpol => {
                return self.parse_interpolated_string(NixString::Literal(initial_content));
            }
            t => return unexpected(t),
        };
        let mut parts = vec![initial_content, next_part];

        loop {
            match self.expect_next()? {
                Token::StringEnd => return Ok(NixString::Composite(parts)),
                Token::StringContent(cont) => {
                    parts.push(cont);
                }
                Token::BeginInterpol => {
                    return self.parse_interpolated_string(NixString::Composite(parts));
                }
                t => return unexpected(t),
            }
        }
    }
}
