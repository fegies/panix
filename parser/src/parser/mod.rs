use std::iter::Peekable;

use lexer::{LexError, Token};

use self::{ast::NixExpr, components::Parser};

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

pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

#[derive(Debug)]
pub enum ParseError<'a> {
    UnexpectedToken(Token<'a>),
    UnexpectedEof,
    LexerError(LexError),
}
impl From<LexError> for ParseError<'_> {
    fn from(value: LexError) -> Self {
        Self::LexerError(value)
    }
}

pub fn parse_nix(input: &[u8]) -> ParseResult<NixExpr> {
    lexer::run(input, |tokens| {
        let source = tokens.peekable();
        Parser::new(source).run()
    })?
}
