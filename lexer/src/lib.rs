use core::str::Utf8Error;
use std::fmt::{Display, Pointer};

mod lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Ident(&'a str),
    Float(f64),
    Integer(i64),
    At,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    Ampersand,
    SquareClose,
    Dot,
    TripleDot,
    Slash,
    DoubleSlash,
    Eq,
    DoubleEq,
    Colon,
    Semicolon,
    Plus,
    DoublePlus,
    Minus,
    Star,
    RoundOpen,
    RoundClose,
    KwLet,
    KwIn,
    KwWith,
    KwRec,
    KwNull,
    KwAssert,
    Comma,
    StringBegin,
    StringEnd,
    IndentedStringBegin,
    PathBegin,
    PathEnd,
    StringContent(&'a str),
    BeginInterpol,
    EndInterpol,
    Implication,
    Lt,
    Le,
    Gt,
    Ge,
    Not,
    Ne,
    Or,
    And,
    QuestionMark,
    EOF,
    Whitespace,
    KwIf,
    KwElse,
    KwThen,
    KwInherit,
}
impl Eq for Token<'_> {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SourcePosition {
    pub line: u32,
    pub column: u32,
}
impl Display for SourcePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line, self.column))
    }
}

#[derive(Debug)]
pub struct TokenWithPosition<'a> {
    pub token: Token<'a>,
    pub position: SourcePosition,
}

impl<'a> AsRef<Token<'a>> for TokenWithPosition<'a> {
    fn as_ref(&self) -> &Token<'a> {
        &self.token
    }
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum LexError {
    #[error("Found an unexpected character: `{0:?}`")]
    UnexpectedChar(Option<u8>),
    #[error("Found an invalid character: `{char}` at {pos}")]
    InvalidChar { char: u8, pos: SourcePosition },
    #[error("Found an invalid escape sequence entry: `{0}`")]
    InvalidEscapeSequence(u8),
    #[error("Found an unclosed string")]
    UnclosedString,
    #[error("Found an invalid string: ")]
    InvalidString(#[from] Utf8Error),
    #[error("Found an unmatched closing brace")]
    UnmatchedCloseBrace,
    #[error("Lexer could not complete")]
    NotRunToCompletion,
    #[error("Exceeded maximum brace recursion")]
    NestedTooDeep,
    #[error("Found an invalid float")]
    InvalidFloat,
    #[error("Found an invalid int")]
    InvalidInt,
}

pub use lexer::run;
