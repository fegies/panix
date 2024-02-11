use std::iter::Peekable;

use lexer::{LexError, Token};

use self::{ast::NixExpr, components::parse_expr};

pub mod ast;
mod components;

trait TokenSource<'a> {
    fn next(&mut self) -> Option<Token<'a>>;
    fn peek(&mut self) -> Option<&Token<'a>>;

    fn expect_next(&mut self) -> ParseResult<Token<'a>> {
        self.next().ok_or(ParseError::UnexpectedEof)
    }

    fn expect(&mut self, token: Token<'static>) -> ParseResult<()> {
        let next = self.expect_next()?;
        if next == token {
            Ok(())
        } else {
            let token = format!("{:?}", token);
            Err(ParseError::UnexpectedToken(token))
        }
    }
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

fn parse_file<'t>(source: &mut impl TokenSource<'t>) -> ParseResult<NixExpr<'t>> {
    let res = parse_expr(source)?;
    source.expect(Token::EOF)?;
    Ok(res)
}

pub fn parse_nix(input: &[u8]) -> ParseResult<NixExpr> {
    lexer::run(input, |tokens| {
        let mut source = tokens.peekable();
        parse_file(&mut source)
    })?
}
