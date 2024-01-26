use std::str::Utf8Error;

use lexer_impedance_matcher::ImpedanceMatcher;
use memchr::memmem;

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub enum LexError {
    UnexpectedChar(Option<u8>),
    InvalidChar(u8),
    UnclosedString,
    InvalidString(Utf8Error),
    UnmatchedCloseBrace,
}

enum BraceStackEntry {
    Normal,
    Interpolation,
}

struct Lexer<'input, 'matcher> {
    input: &'input [u8],
    matcher: &'matcher ImpedanceMatcher<Token<'input>>,
    brace_stack: Vec<BraceStackEntry>,
}

const fn is_pathchar(char: u8) -> bool {
    todo!()
}

macro_rules! whitespace_pat {
    () => {
        b' ' | b'\t' | b'\n' | b'\r'
    };
}
macro_rules! ident_pat {
    (strip_minus) => {
        b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_'
    };
    () => {
        ident_pat!(strip_minus) | b'-'
    }
}

impl<'a> Lexer<'a, '_> {
    async fn run(mut self) -> Result<(), LexError> {
        self.lex_normal().await
    }

    async fn push(&mut self, token: Token<'a>) {
        println!("{token:?}");
        self.matcher.push(token).await
    }

    async fn lex_normal(&mut self) -> Result<(), LexError> {
        macro_rules! single {
            ($token: expr) => {{
                self.push($token).await;
                self.input = &self.input[1..];
            }};
        }
        macro_rules! lookahead {
            ($byte: expr, $match_token: expr, $fallback_token: expr) => {{
                if Some($byte) == self.input.get(1).cloned() {
                    self.push($match_token);
                    self.input = &self.input[2..];
                } else {
                    single!($fallback_token);
                }
            }};
        }
        while let Some(cur) = self.input.first() {
            match *cur {
                whitespace_pat!() => self.skip_whitespace(),
                b';' => single!(Token::Semicolon),
                b'@' => single!(Token::At),
                b'*' => single!(Token::Star),
                b'[' => single!(Token::SquareOpen),
                b']' => single!(Token::SquareClose),
                b'(' => single!(Token::RoundOpen),
                b')' => single!(Token::RoundClose),
                b'?' => single!(Token::QuestionMark),
                b'{' => {
                    self.brace_stack.push(BraceStackEntry::Normal);
                    single!(Token::CurlyOpen)
                }
                b'}' => {
                    let top = self
                        .brace_stack
                        .pop()
                        .ok_or(LexError::UnmatchedCloseBrace)?;
                    match top {
                        BraceStackEntry::Interpolation => {
                            self.input = &self.input[1..];
                            self.push(Token::EndInterpol);
                            return Ok(());
                        }
                        BraceStackEntry::Normal => {
                            single!(Token::CurlyClose)
                        }
                    }
                }
                b':' => single!(Token::Colon),
                b',' => single!(Token::Comma),
                b'+' => lookahead!(b'+', Token::DoublePlus, Token::Plus),
                b'-' => lookahead!(b'>', Token::Implication, Token::Minus),
                b'=' => lookahead!(b'=', Token::DoubleEq, Token::Eq),
                b'<' => lookahead!(b'=', Token::Le, Token::Lt),
                b'>' => lookahead!(b'=', Token::Ge, Token::Gt),
                b'!' => lookahead!(b'=', Token::Ne, Token::Not),
                b'"' => self.lex_simple_string()?,
                b'#' => self.skip_comment(),
                b'|' => {
                    let next = self.input.get(1).cloned();
                    if Some(b'|') == next {
                        self.push(Token::Or);
                        self.input = &self.input[2..];
                    } else {
                        return Err(LexError::UnexpectedChar(next));
                    }
                }
                b'.' => {
                    if Some(b"...".as_ref()) == self.input.get(..3) {
                        self.push(Token::TripleDot);
                        self.input = &self.input[3..];
                    } else if Some(&b'/') == self.input.get(1) {
                        self.lex_path().await?;
                    } else {
                        single!(Token::Dot)
                    }
                }
                b'/' => {
                    match self.input.get(1).cloned() {
                        Some(b'/') => {
                            self.push(Token::DoubleSlash);
                            self.input = &self.input[2..];
                        }
                        Some(b'*') => {
                            // we have to skip a multiline comment
                            todo!()
                        }
                        Some(c) if is_pathchar(c) => self.lex_path().await?,
                        _ => single!(Token::Slash),
                    }
                }
                b'i' if self.try_parse_keyword(b"in", Token::KwIn).is_some() => {}
                b'l' if self.try_parse_keyword(b"let", Token::KwLet).is_some() => {}
                b'r' if self.try_parse_keyword(b"rec", Token::KwRec).is_some() => {}
                b'w' if self.try_parse_keyword(b"with", Token::KwWith).is_some() => {}
                b'n' if self.try_parse_keyword(b"null", Token::KwNull).is_some() => {}
                b'\'' => todo!(),
                ident_pat!(strip_minus) => self.lex_ident()?,
                c => {
                    println!("{:?}", std::char::from_u32(c as u32));
                    return Err(LexError::InvalidChar(c));
                }
            }
        }
        Ok(())
    }

    fn lex_ident(&mut self) -> Result<(), LexError> {
        let end_idx = self
            .input
            .iter()
            .enumerate()
            .find(|(_idx, char)| match char {
                ident_pat!() => false,
                _ => true,
            });
        let ident_slice;
        if let Some((end_idx, _)) = end_idx {
            ident_slice = self.consume(end_idx);
        } else {
            ident_slice = self.input;
            self.input = &[];
        }
        let ident = Self::convert_str(ident_slice)?;
        self.push(Token::Ident(ident));
        Ok(())
    }

    fn try_parse_keyword(&mut self, keyword: &'static [u8], token: Token<'a>) -> Option<()> {
        let kw_len = keyword.len();
        if keyword != self.input.get(..kw_len)? {
            return None;
        }

        match self.input.get(kw_len) {
            // the keyword is followed by a character that is valid in an ident, so it is not a keyword here
            Some(ident_pat!()) => None,
            _ => {
                // totally a keyword :)
                self.push(token);
                self.input = &self.input[kw_len..];
                Some(())
            }
        }
    }

    fn convert_str(content: &[u8]) -> Result<&str, LexError> {
        std::str::from_utf8(content).map_err(LexError::InvalidString)
    }
    fn consume(&mut self, length: usize) -> &'a [u8] {
        let res = &self.input[..length];
        self.input = &self.input[length..];
        res
    }
    fn consume_as_str(&mut self, length: usize) -> Result<&'a str, LexError> {
        Self::convert_str(self.consume(length))
    }

    async fn lex_path(&mut self) -> Result<(), LexError> {
        self.push(Token::PathBegin);
        'outer: loop {
            let mut end_idx = None;
            for (idx, char) in self.input.iter().cloned().enumerate() {
                match char {
                    ident_pat!() | b'/' | b'.' => {}
                    b'$' if matches!(self.input.get(idx + 1), Some(&b'{')) => {
                        let str = self.consume_as_str(idx)?;
                        self.input = &self.input[2..];
                        self.push(Token::StringContent(str));
                        self.lex_interpolation().await?;
                        continue 'outer;
                    }
                    _ => {
                        end_idx = Some(idx);
                        break;
                    }
                }
            }

            let final_part;
            if let Some(end_idx) = end_idx {
                final_part = self.consume(end_idx);
            } else {
                final_part = self.input;
                self.input = &[];
            }
            let final_part = Self::convert_str(final_part)?;
            self.push(Token::StringContent(final_part));
            break;
        }
        self.push(Token::PathEnd);
        Ok(())
    }

    async fn lex_interpolation(&mut self) -> Result<(), LexError> {
        self.push(Token::BeginInterpol);
        // interpolation counts as an opening brace
        self.brace_stack.push(BraceStackEntry::Interpolation);
        Ok(())
    }

    fn push_string_content(&mut self, content: &'a [u8]) -> Result<(), LexError> {
        if memchr::memchr(b'\\', content).is_some() {
            todo!()
        }
        let content = Self::convert_str(content)?;
        self.push(Token::StringContent(content));
        Ok(())
    }

    fn lex_simple_string(&mut self) -> Result<(), LexError> {
        // skip the starting quotation mark
        self.input = &self.input[1..];
        self.push(Token::StringBegin);

        for quot_pos in memchr::memchr_iter(b'"', self.input) {
            if quot_pos == 0 {
                // this is actually an empty string :)
                self.push(Token::StringEnd);
                self.input = &self.input[1..];
                return Ok(());
            }

            if self.input[quot_pos - 1] == b'\\' {
                continue;
            }

            // we found an unescaped quotation mark.
            // let's see if there is an interpolation going on
            let candidate = &self.input[..quot_pos];
            if memmem::find(candidate, b"${").is_some() {
                todo!();
            } else {
                self.push_string_content(candidate)?;
                self.push(Token::StringEnd);
                self.input = &self.input[quot_pos + 1..];
                return Ok(());
            }
        }

        Err(LexError::UnclosedString)
    }
    fn skip_whitespace(&mut self) {
        loop {
            match self.input.first() {
                Some(whitespace_pat!()) => {
                    self.input = &self.input[1..];
                }
                _ => break,
            }
        }
    }

    fn skip_comment(&mut self) {
        if let Some(endline_pos) = memchr::memchr(b'\n', self.input) {
            self.input = &self.input[endline_pos..];
        } else {
            self.input = &[];
        }
    }
}

pub fn lex_input(input: &[u8]) -> Result<Vec<Token>, LexError> {
    let adapter = ImpedanceMatcher::new();
    let future = Lexer {
        input,
        matcher: &adapter,
        brace_stack: Vec::new(),
    }
    .run();
    let vec: Vec<_> = adapter.run(future, |iter| iter.collect());
    Ok(vec)
}
