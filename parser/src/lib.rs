pub mod ast;
pub mod parser;

#[cfg(test)]
mod tests;

use parser::parser_entrypoint;

pub use lexer::LexError;
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Found an unexpected token: `{0}`")]
    UnexpectedToken(String),
    #[error("File ended unexpectedly")]
    UnexpectedEof,
    #[error("Error while lexing")]
    LexerError(#[from] LexError),
    #[error("Found conflicting attributes: {0}")]
    AttributePathConflict(String),
}

pub fn parse_nix(input: &[u8]) -> ParseResult<ast::NixExpr<'_>> {
    parser_entrypoint(input)
}
