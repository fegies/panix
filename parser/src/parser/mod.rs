use std::iter::Peekable;

use lexer::{LexError, Token};

use self::{ast::NixExpr, components::parse_complete};

pub mod ast;
mod components;

trait TokenSource<'a> {
    fn next(&mut self) -> Option<Token<'a>>;
    fn peek(&mut self) -> Option<&Token<'a>>;
}

impl<'a, I> TokenSource<'a> for Peekable<I>
where
    I: Iterator<Item = Token<'a>>,
{
    fn next(&mut self) -> Option<Token<'a>> {
        Iterator::next(self)
    }

    fn peek(&mut self) -> Option<&Token<'a>> {
        Peekable::peek(self)
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    UnexpectedEof,
    LexerError(LexError),
}
impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        Self::LexerError(value)
    }
}

pub fn parse_nix(input: &[u8]) -> ParseResult<NixExpr> {
    let res = lexer::run(input, |tokens| {
        let source = tokens.peekable();
        parse_complete(source)
    });

    match res {
        Ok(Ok(e)) => Ok(e),
        Err((lex, Ok(_))) => Err(lex.into()),
        Err((_, Err(e))) => Err(e),
        Ok(Err(e)) => Err(e),
    }
}
