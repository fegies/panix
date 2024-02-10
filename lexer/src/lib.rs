#![cfg_attr(not(test), no_std)]

use core::str::Utf8Error;

mod lexer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Ident(&'a str),
    At,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
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
    QuestionMark,
    EOF,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LexError {
    UnexpectedChar(Option<u8>),
    InvalidChar(u8),
    UnclosedString,
    InvalidString(Utf8Error),
    UnmatchedCloseBrace,
    NotRunToCompletion,
    NestedTooDeep,
}

pub use lexer::run;
