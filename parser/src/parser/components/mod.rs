pub mod expr;
pub mod function;
pub mod let_expr;
pub mod list;
pub mod string;

use lexer::Token;

use super::{ParseError, ParseResult, TokenSource};

use super::ast::*;

pub fn parse_complete<'t>(source: impl TokenSource<'t>) -> ParseResult<NixExpr<'t>> {
    let mut parser = Parser { source };
    let res = parser.parse_expr()?;
    parser.expect(Token::EOF)?;
    Ok(res)
}

struct Parser<S> {
    source: S,
}

impl<'t, S: TokenSource<'t>> Parser<S> {
    fn next(&mut self) -> Option<Token<'t>> {
        self.source.next()
    }
    fn peek(&mut self) -> Option<&Token<'t>> {
        self.source.peek()
    }

    fn expect_next_or_whitespace(&mut self) -> ParseResult<Token<'t>> {
        self.next().ok_or(ParseError::UnexpectedEof)
    }

    fn expect_next(&mut self) -> ParseResult<Token<'t>> {
        loop {
            let token = self.expect_next_or_whitespace()?;
            if Token::Whitespace != token {
                return Ok(token);
            }
        }
    }

    fn expect_peek_or_whitespace(&mut self) -> ParseResult<&Token<'t>> {
        self.peek().ok_or(ParseError::UnexpectedEof)
    }

    fn expect_peek(&mut self) -> ParseResult<&Token<'t>> {
        while let Some(Token::Whitespace) = self.peek() {
            self.next();
        }
        self.peek().ok_or(ParseError::UnexpectedEof)
    }

    fn expect(&mut self, token: Token<'static>) -> ParseResult<()> {
        let next = self.expect_next()?;
        if next == token {
            Ok(())
        } else {
            unexpected_with_expected(next, token)
        }
    }
}

#[cold]
fn unexpected<T>(t: Token) -> ParseResult<T> {
    let token = format!("{:?}", t);
    Err(super::ParseError::UnexpectedToken(token))
}
#[cold]
fn unexpected_with_expected<T>(t: Token, expected: Token) -> ParseResult<T> {
    let token = format!("{t:?}, expected {expected:?}");
    Err(super::ParseError::UnexpectedToken(token))
}
