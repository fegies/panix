pub mod attrset;
pub mod expr;
pub mod function;
pub mod if_expr;
pub mod let_expr;
pub mod list;
pub mod string;
mod with;

use std::backtrace::{self, Backtrace};

use lexer::{Token, TokenWithPosition};

use super::{ParseError, ParseResult, TokenSource};

use super::ast::*;

pub fn parse_complete<'t>(source: impl TokenSource<'t>) -> ParseResult<NixExpr<'t>> {
    let mut parser = Parser { source };
    let res = parser.parse_expr()?;
    parser.expect(Token::EOF)?;
    assert!(parser.next().is_none());
    Ok(res)
}

struct Parser<S> {
    source: S,
}

impl<'t, S: TokenSource<'t>> Parser<S> {
    fn next(&mut self) -> Option<TokenWithPosition<'t>> {
        self.source.next()
    }
    fn peek(&mut self) -> Option<&Token<'t>> {
        self.source.peek().map(|t| &t.token)
    }
    fn peek_no_whitespace(&mut self) -> Option<Token<'t>> {
        match self.source.peek().map(|t| t.token.clone()) {
            Some(Token::Whitespace) => self.source.peek_2().map(|t| t.token.clone()),
            t => t,
        }
    }

    fn expect_next_or_whitespace(&mut self) -> ParseResult<TokenWithPosition<'t>> {
        self.next().ok_or_else(unexpected_eof)
    }

    fn expect_next(&mut self) -> ParseResult<TokenWithPosition<'t>> {
        loop {
            let token = self.expect_next_or_whitespace()?;
            if Token::Whitespace != token.token {
                return Ok(token);
            }
        }
    }
    fn expect_peek(&mut self) -> ParseResult<&Token<'t>> {
        match self.peek().ok_or_else(unexpected_eof)? {
            Token::Whitespace => self
                .source
                .peek_2()
                .map(|t| &t.token)
                .ok_or_else(unexpected_eof),
            _ => Ok(self.peek().expect("cannot be empty")),
        }
    }

    fn expect(&mut self, token: Token<'static>) -> ParseResult<()> {
        let next = self.expect_next()?;
        if next.token == token {
            Ok(())
        } else {
            unexpected_with_expected(next, token)
        }
    }

    fn expect_ident(&mut self) -> ParseResult<&'t str> {
        let next = self.expect_next()?;
        if let Token::Ident(ident) = next.token {
            Ok(ident)
        } else {
            unexpected(next)
        }
    }
}

#[cold]
fn unexpected_eof() -> ParseError {
    println!(
        "unexpected eof at: \n{}",
        format_backtrace(std::backtrace::Backtrace::capture())
    );
    ParseError::UnexpectedEof
}

#[cold]
fn unexpected<T>(t: TokenWithPosition) -> ParseResult<T> {
    let t = t;
    let token = format!(
        "{t:?} at \n{}",
        format_backtrace(std::backtrace::Backtrace::capture())
    );
    Err(super::ParseError::UnexpectedToken(token))
}
#[cold]
fn unexpected_with_expected<T>(t: TokenWithPosition, expected: Token) -> ParseResult<T> {
    let token = format!(
        "{t:?}, expected {expected:?} at \n{}",
        format_backtrace(Backtrace::capture())
    );
    Err(super::ParseError::UnexpectedToken(token))
}
#[inline]
fn format_backtrace(backtrace: Backtrace) -> String {
    let backtrace = format!("{}", backtrace);
    let res = backtrace
        .lines()
        .take_while(|l| !l.contains("parser::parser::parser_entrypoint"))
        .map(|l| format!("{l}\n"))
        .collect();
    println!("{res}");
    res
}
