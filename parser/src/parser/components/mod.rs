pub mod let_expr;
pub mod string;

use lexer::Token;

use super::{ParseResult, TokenSource};

use super::ast::*;

pub fn parse_expr<'t>(source: &mut impl TokenSource<'t>) -> ParseResult<NixExpr<'t>> {
    todo!()
}

// pub struct Parser<S> {
//     source: S,
// }

fn unexpected<T>(t: Token) -> ParseResult<T> {
    let token = format!("{:?}", t);
    Err(super::ParseError::UnexpectedToken(token))
}

// impl<'t, S> Parser<S>
// where
//     S: TokenSource<'t>,
// {
//     pub fn new(source: S) -> Self {
//         Self { source }
//     }

//     pub fn run(mut self) -> ParseResult<'t, NixExpr<'t>> {
//         let res = self.parse_expr()?;
//         self.expect(Token::EOF)?;
//         Ok(res)
//     }

//     fn parse_expr(&mut self) -> ParseResult<'t, NixExpr<'t>> {
//         match self.expect_next()? {
//             Token::StringBegin => self
//                 .parse_simple_string()
//                 .map(BasicValue::String)
//                 .map(NixExpr::BasicValue),
//             Token::Ident(ident) => {
//                 println!("found {ident:?}");
//                 todo!()
//             }
//             Token::At => todo!(),
//             Token::CurlyOpen => todo!(),
//             Token::CurlyClose => todo!(),
//             Token::SquareOpen => todo!(),
//             Token::SquareClose => todo!(),
//             Token::Dot => todo!(),
//             Token::TripleDot => todo!(),
//             Token::Slash => todo!(),
//             Token::DoubleSlash => todo!(),
//             Token::Eq => todo!(),
//             Token::DoubleEq => todo!(),
//             Token::Colon => todo!(),
//             Token::Semicolon => todo!(),
//             Token::Plus => todo!(),
//             Token::DoublePlus => todo!(),
//             Token::Minus => todo!(),
//             Token::Star => todo!(),
//             Token::RoundOpen => todo!(),
//             Token::RoundClose => todo!(),
//             Token::KwLet => todo!(),
//             Token::KwIn => todo!(),
//             Token::KwWith => todo!(),
//             Token::KwRec => todo!(),
//             Token::KwNull => todo!(),
//             Token::Comma => todo!(),
//             Token::StringEnd => todo!(),
//             Token::IndentedStringBegin => todo!(),
//             Token::PathBegin => todo!(),
//             Token::PathEnd => todo!(),
//             Token::StringContent(_) => todo!(),
//             Token::BeginInterpol => todo!(),
//             Token::EndInterpol => todo!(),
//             Token::Implication => todo!(),
//             Token::Lt => todo!(),
//             Token::Le => todo!(),
//             Token::Gt => todo!(),
//             Token::Ge => todo!(),
//             Token::Not => todo!(),
//             Token::Ne => todo!(),
//             Token::Or => todo!(),
//             Token::QuestionMark => todo!(),
//             Token::EOF => todo!(),
//         }
//     }

// }
