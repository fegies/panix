use lexer::{LexError, TokenWithPosition};

use self::{ast::NixExpr, components::parse_complete, util::multipeek::Multipeek};

pub mod ast;
mod components;
mod util;

trait TokenSource<'a> {
    fn next(&mut self) -> Option<TokenWithPosition<'a>>;
    fn peek(&mut self) -> Option<&TokenWithPosition<'a>>;
    fn peek_2(&mut self) -> Option<&TokenWithPosition<'a>>;
}

impl<'a, I> TokenSource<'a> for Multipeek<I>
where
    I: Iterator<Item = TokenWithPosition<'a>>,
{
    fn next(&mut self) -> Option<TokenWithPosition<'a>> {
        Iterator::next(self)
    }

    fn peek(&mut self) -> Option<&TokenWithPosition<'a>> {
        Multipeek::peek(self)
    }

    fn peek_2(&mut self) -> Option<&TokenWithPosition<'a>> {
        Multipeek::peek_2(self)
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    UnexpectedEof,
    LexerError(LexError),
    AttributePathConflict(String),
}
impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        Self::LexerError(value)
    }
}

fn parse_nix_inner(input: &[u8]) -> ParseResult<NixExpr> {
    let res = lexer::run(input, |tokens| {
        let source = Multipeek::new(tokens);
        parse_complete(source)
    });

    match res {
        Ok(Ok(e)) => Ok(e),
        Err((lex, Ok(_))) => Err(lex.into()),
        Err((_, Err(e))) => Err(e),
        Ok(Err(e)) => Err(e),
    }
}

// this is just a convenient point to cut off the stack trace.
fn parser_entrypoint(input: &[u8]) -> ParseResult<NixExpr> {
    parse_nix_inner(input)
}

pub fn parse_nix(input: &[u8]) -> ParseResult<NixExpr> {
    parser_entrypoint(input)
}
