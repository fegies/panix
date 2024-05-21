pub mod ast;
pub mod parser;

use lexer::LexError;
use parser::parser_entrypoint;

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

pub fn parse_nix(input: &[u8]) -> ParseResult<ast::NixExpr> {
    parser_entrypoint(input)
}
