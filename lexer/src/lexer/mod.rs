mod util;

mod stackstack;
#[cfg(test)]
mod tests;

use lexer_impedance_matcher::{ImpedanceMatcher, IteratorAdapter};
use memchr::memmem;

use crate::{lexer::util::LineSplitter, LexError, Token};

use self::{stackstack::Stack, util::ends_with_unsecaped_backslash};

enum BraceStackEntry {
    Normal,
    SimpleStringInterpolation,
    MultilineStringInterpolation,
    PathInterpolation,
}

struct Lexer<'input, 'matcher> {
    input: &'input [u8],
    matcher: &'matcher ImpedanceMatcher<Token<'input>>,
    brace_stack: Stack<128, BraceStackEntry>,
}

const fn is_pathchar(char: u8) -> bool {
    todo!()
}

macro_rules! whitespace_pat {
    () => {
        whitespace_pat!(no_newline) | b'\n'
    };
    (no_newline) => {
        b' ' | b'\t' | b'\r'
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

impl<'a, 'matcher> Lexer<'a, 'matcher> {
    pub fn new(input: &'a [u8], matcher: &'matcher ImpedanceMatcher<Token<'a>>) -> Self {
        Self {
            input,
            matcher,
            brace_stack: Stack::new(),
        }
    }

    async fn run(mut self) -> Result<(), LexError> {
        self.lex_normal().await
    }

    async fn push(&mut self, token: Token<'a>) {
        self.matcher.push(token).await
    }
    fn push_brace(&mut self, entry: BraceStackEntry) -> Result<(), LexError> {
        self.brace_stack
            .push(entry)
            .map_err(|_| LexError::NestedTooDeep)
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
                    self.push($match_token).await;
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
                    self.push_brace(BraceStackEntry::Normal)?;
                    single!(Token::CurlyOpen)
                }
                b'}' => {
                    let top = self
                        .brace_stack
                        .pop()
                        .ok_or(LexError::UnmatchedCloseBrace)?;
                    self.consume(1);
                    match top {
                        BraceStackEntry::Normal => self.push(Token::CurlyClose).await,
                        BraceStackEntry::SimpleStringInterpolation => {
                            self.push(Token::EndInterpol).await;
                            self.resume_lex_simple_string().await?;
                        }
                        BraceStackEntry::MultilineStringInterpolation => {
                            self.push(Token::EndInterpol).await;
                            self.resume_lex_multiline_string().await?;
                        }
                        BraceStackEntry::PathInterpolation => {
                            self.push(Token::EndInterpol).await;
                            self.resume_lex_path().await?;
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
                b'"' => self.lex_simple_string().await?,
                b'#' => self.skip_comment(),
                b'|' => {
                    let next = self.input.get(1).cloned();
                    if Some(b'|') == next {
                        self.push(Token::Or).await;
                        self.input = &self.input[2..];
                    } else {
                        return Err(LexError::UnexpectedChar(next));
                    }
                }
                b'.' => {
                    if Some(b"...".as_ref()) == self.input.get(..3) {
                        self.push(Token::TripleDot).await;
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
                            self.push(Token::DoubleSlash).await;
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
                b'i' if self.try_parse_keyword(b"in", Token::KwIn).await.is_some() => {}
                b'l' if self.try_parse_keyword(b"let", Token::KwLet).await.is_some() => {}
                b'r' if self.try_parse_keyword(b"rec", Token::KwRec).await.is_some() => {}
                b'w' if self
                    .try_parse_keyword(b"with", Token::KwWith)
                    .await
                    .is_some() => {}
                b'n' if self
                    .try_parse_keyword(b"null", Token::KwNull)
                    .await
                    .is_some() => {}
                b'\'' if self.input.get(1) == Some(&b'\'') => {
                    self.lex_indented_string().await?;
                }
                ident_pat!(strip_minus) => self.lex_ident().await?,
                c => {
                    return Err(LexError::InvalidChar(c));
                }
            }
        }
        self.push(Token::EOF).await;
        Ok(())
    }

    async fn lex_ident(&mut self) -> Result<(), LexError> {
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
        self.push(Token::Ident(ident)).await;
        Ok(())
    }

    async fn try_parse_keyword(&mut self, keyword: &'static [u8], token: Token<'a>) -> Option<()> {
        let kw_len = keyword.len();
        if keyword != self.input.get(..kw_len)? {
            return None;
        }

        match self.input.get(kw_len) {
            // the keyword is followed by a character that is valid in an ident, so it is not a keyword here
            Some(ident_pat!()) => None,
            _ => {
                // totally a keyword :)
                self.push(token).await;
                self.input = &self.input[kw_len..];
                Some(())
            }
        }
    }

    fn convert_str(content: &[u8]) -> Result<&str, LexError> {
        core::str::from_utf8(content).map_err(LexError::InvalidString)
    }
    #[inline]
    fn consume(&mut self, length: usize) -> &'a [u8] {
        let res = &self.input[..length];
        self.input = &self.input[length..];
        res
    }
    fn consume_as_str(&mut self, length: usize) -> Result<&'a str, LexError> {
        Self::convert_str(self.consume(length))
    }

    async fn lex_path(&mut self) -> Result<(), LexError> {
        self.push(Token::PathBegin).await;
        self.resume_lex_path().await
    }

    async fn resume_lex_path(&mut self) -> Result<(), LexError> {
        let mut end_idx = None;

        for (idx, char) in self.input.iter().cloned().enumerate() {
            match char {
                ident_pat!() | b'/' | b'.' => {}
                b'$' if matches!(self.input.get(idx + 1), Some(&b'{')) => {
                    let str = self.consume_as_str(idx)?;
                    self.input = &self.input[2..];
                    self.push(Token::StringContent(str)).await;
                    self.push(Token::BeginInterpol).await;
                    self.brace_stack.push(BraceStackEntry::PathInterpolation);
                    return Ok(());
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
        self.push(Token::StringContent(final_part)).await;
        self.push(Token::PathEnd).await;
        Ok(())
    }

    async fn push_string_content(&mut self, content: &'a [u8]) -> Result<(), LexError> {
        if memchr::memchr(b'\\', content).is_some() {
            todo!()
        }
        let content = Self::convert_str(content)?;
        self.push(Token::StringContent(content)).await;
        Ok(())
    }

    async fn lex_simple_string(&mut self) -> Result<(), LexError> {
        // skip the starting quotation mark
        self.consume(1);
        self.push(Token::StringBegin).await;
        self.resume_lex_simple_string().await
    }

    async fn lex_indented_string(&mut self) -> Result<(), LexError> {
        // skip starting quot
        self.consume(2);
        self.push(Token::IndentedStringBegin).await;

        // we need to skip all leading whitespace of the first line if it is actually
        // a multiline string
        let num_leading_spaces = self
            .input
            .iter()
            .take_while(|c| matches!(**c, whitespace_pat!(no_newline)))
            .count();
        if self.input.get(num_leading_spaces) == Some(&b'\n') {
            // it is a multiline string!
            self.consume(num_leading_spaces + 1);
        }

        self.resume_lex_multiline_string().await
    }

    async fn resume_lex_multiline_string(&mut self) -> Result<(), LexError> {
        'outer: loop {
            let mut search_idx = 0;
            while let Some(idx) = memchr::memchr3(b'\n', b'$', b'\'', &self.input[search_idx..]) {
                let decide_idx = search_idx + idx;
                match self.input[decide_idx] {
                    b'\n' => {
                        let part = self.consume_as_str(decide_idx + 1)?;
                        self.push(Token::StringContent(part)).await;
                        continue 'outer;
                    }
                    b'$' if self.input.get(decide_idx + 1) == Some(&b'{') => {
                        let part = self.consume_as_str(decide_idx)?;
                        if part.len() > 0 {
                            self.push(Token::StringContent(part)).await;
                        }
                        // skip the opening dollar brace
                        self.consume(2);
                        self.push(Token::BeginInterpol).await;
                        self.push_brace(BraceStackEntry::MultilineStringInterpolation)?;
                        return Ok(());
                    }
                    b'\'' if self.input.get(decide_idx + 1) == Some(&b'\'') => {
                        match self.input.get(decide_idx + 2).cloned() {
                            Some(b'$' | b'\'') => {
                                // we found something that is escaped
                                search_idx = decide_idx + 3;
                            }
                            _ => {
                                // end of string
                                let part = self.consume_as_str(decide_idx)?;
                                if part.len() > 0 {
                                    self.push(Token::StringContent(part)).await;
                                }
                                self.push(Token::StringEnd).await;

                                // skip the string close quotes
                                self.consume(2);

                                return Ok(());
                            }
                        }
                    }
                    _ => search_idx = decide_idx + 1,
                }
            }
            break;
        }

        Err(LexError::UnclosedString)
    }

    async fn resume_lex_simple_string(&mut self) -> Result<(), LexError> {
        for quot_pos in memchr::memchr_iter(b'"', self.input) {
            if quot_pos == 0 {
                // this is actually an empty string :)
                self.push(Token::StringEnd).await;
                self.input = &self.input[1..];
                return Ok(());
            }

            if self.input[quot_pos - 1] == b'\\' {
                continue;
            }

            // we found an unescaped quotation mark.
            // let's see if there is an interpolation going on
            let candidate = &self.input[..quot_pos];

            for idx in memmem::find_iter(candidate, b"${") {
                let part_to_beginning_of_interpolation = &candidate[..idx];

                if !ends_with_unsecaped_backslash(part_to_beginning_of_interpolation) {
                    // we actually found an interpolation!
                    self.push_string_content(part_to_beginning_of_interpolation)
                        .await?;
                    self.consume(part_to_beginning_of_interpolation.len() + 2);
                    self.push_brace(BraceStackEntry::SimpleStringInterpolation)?;
                }
            }

            self.push_string_content(candidate).await?;
            self.push(Token::StringEnd).await;
            self.input = &self.input[quot_pos + 1..];
            return Ok(());
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

pub fn run<'a, TRes>(
    input: &'a [u8],
    consumer: impl FnOnce(IteratorAdapter<'_, '_, Token<'a>, Result<(), LexError>>) -> TRes,
) -> Result<TRes, LexError> {
    let adapter = ImpedanceMatcher::new();

    let future = Lexer {
        input,
        matcher: &adapter,
        brace_stack: Stack::new(),
    }
    .run();

    let (lex_res, consumer_res) = adapter.run(future, consumer);

    lex_res
        .unwrap_or(Err(LexError::NotRunToCompletion))
        .and(Ok(consumer_res))
}

// pub fn run_lexer<'a, TFConsumer, R>(
//     input: &'a [u8],
//     consumer: impl TokenConsumer<'a, R>,
// ) -> Result<R, LexError> {
//     let matcher = ImpedanceMatcher::new();
//     let future = Lexer {
//         input,
//         matcher: &matcher,
//         brace_stack: Stack::new(),
//     }
//     .run();

//     let (lexer_res, res) = matcher.run(future, consumer);
// }
