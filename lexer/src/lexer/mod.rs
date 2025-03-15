mod util;

mod input;
mod stackstack;
#[cfg(test)]
mod tests;

use lexer_impedance_matcher::{ImpedanceMatcher, IteratorAdapter};
use memchr::memmem;

use crate::{LexError, SourcePosition, Token, TokenWithPosition};

use self::{input::LexerInput, stackstack::Stack, util::ends_with_unsecaped_backslash};

enum BraceStackEntry {
    Normal,
    SimpleStringInterpolation,
    MultilineStringInterpolation,
    PathInterpolation,
    UnquotedInterpolation,
}

struct Lexer<'input, 'matcher> {
    input: LexerInput<'input>,
    matcher: &'matcher ImpedanceMatcher<TokenWithPosition<'input>>,
    brace_stack: Stack<128, BraceStackEntry>,
    last_was_whitespace: bool,
}

const fn is_pathchar(char: u8) -> bool {
    matches!(char, b'A'..=b'z' | b'0'..=b'9')
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
        b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'\''
    };
    () => {
        ident_pat!(strip_minus) | b'-' | b'0'..=b'9'
    }
}

impl<'a, 'matcher> Lexer<'a, 'matcher> {
    async fn run(mut self) -> Result<(), LexError> {
        self.lex_normal().await
    }

    async fn push(&mut self, token: Token<'a>) {
        self.push_pos(token, self.input.pos()).await
    }

    async fn push_pos(&mut self, token: Token<'a>, position: crate::SourcePosition) {
        // ensure the lexer swallows consecutive whitespaces
        if token == Token::Whitespace {
            if self.last_was_whitespace {
                return;
            } else {
                self.last_was_whitespace = true;
            }
        } else {
            self.last_was_whitespace = false;
        }
        self.matcher
            .push(TokenWithPosition { token, position })
            .await
    }
    fn push_brace(&mut self, entry: BraceStackEntry) -> Result<(), LexError> {
        self.brace_stack
            .push(entry)
            .map_err(|_| LexError::NestedTooDeep)
    }

    async fn lex_normal(&mut self) -> Result<(), LexError> {
        macro_rules! single {
            ($token: expr_2021) => {{
                self.push($token).await;
                self.input.advance_one();
            }};
        }
        macro_rules! lookahead {
            ($byte: expr_2021, $match_token: expr_2021, $fallback_token: expr_2021) => {{
                if Some($byte) == self.input.get(1) {
                    self.push($match_token).await;
                    self.input.consume(2);
                } else {
                    single!($fallback_token);
                }
            }};
        }
        while let Some(cur) = self.input.get(0) {
            match cur {
                whitespace_pat!() => self.skip_whitespace().await,
                b';' => single!(Token::Semicolon),
                b'@' => single!(Token::At),
                b'*' => single!(Token::Star),
                b'[' => single!(Token::SquareOpen),
                b']' => single!(Token::SquareClose),
                b'(' => single!(Token::RoundOpen),
                b')' => single!(Token::RoundClose),
                b'?' => single!(Token::QuestionMark),
                b'&' => lookahead!(b'&', Token::And, Token::Ampersand),
                b'{' => {
                    self.push_brace(BraceStackEntry::Normal)?;
                    single!(Token::CurlyOpen)
                }
                b'}' => {
                    let top = self
                        .brace_stack
                        .pop()
                        .ok_or(LexError::UnmatchedCloseBrace)?;

                    let pos = self.input.pos();
                    self.input.advance_one();

                    match top {
                        BraceStackEntry::Normal => {
                            self.push_pos(Token::CurlyClose, pos).await;
                        }
                        BraceStackEntry::SimpleStringInterpolation => {
                            self.push_pos(Token::EndInterpol, pos).await;
                            self.resume_lex_simple_string().await?;
                        }
                        BraceStackEntry::MultilineStringInterpolation => {
                            self.push_pos(Token::EndInterpol, pos).await;
                            self.resume_lex_multiline_string().await?;
                        }
                        BraceStackEntry::PathInterpolation => {
                            self.push_pos(Token::EndInterpol, pos).await;
                            self.resume_lex_path().await?;
                        }
                        BraceStackEntry::UnquotedInterpolation => {
                            self.push_pos(Token::EndInterpol, pos).await;
                            self.push_pos(Token::StringEnd, pos).await;
                        }
                    }
                }
                b':' => single!(Token::Colon),
                b',' => single!(Token::Comma),
                b'+' => lookahead!(b'+', Token::DoublePlus, Token::Plus),
                b'-' => lookahead!(b'>', Token::Implication, Token::Minus),
                b'=' => lookahead!(b'=', Token::DoubleEq, Token::Eq),
                b'<' => self.lex_lt_le_or_searchpath().await,
                b'>' => lookahead!(b'=', Token::Ge, Token::Gt),
                b'!' => lookahead!(b'=', Token::Ne, Token::Not),
                b'"' => self.lex_simple_string().await?,
                b'#' => self.skip_comment(),
                b'|' => {
                    let next = self.input.get(1);
                    if Some(b'|') == next {
                        self.push(Token::Or).await;
                        self.input.consume(2);
                    } else {
                        return Err(LexError::UnexpectedChar(next));
                    }
                }
                b'.' if self.input.matches("...") => {
                    self.push(Token::TripleDot).await;
                    self.input.consume(3);
                }
                b'.' if self.input.matches("./") || self.input.matches("../") => {
                    self.lex_path().await?
                }
                b'.' => {
                    single!(Token::Dot)
                }
                b'/' => {
                    match self.input.get(1) {
                        Some(b'/') => {
                            self.push(Token::DoubleSlash).await;
                            self.input.consume(2);
                        }
                        Some(b'*') => {
                            // we have to skip a multiline comment
                            self.input.consume(2);
                            self.skip_multiline_comment();
                        }
                        Some(c) if is_pathchar(c) => self.lex_path().await?,
                        _ => single!(Token::Slash),
                    }
                }
                b'i' if self
                    .try_parse_keyword("inherit", Token::KwInherit)
                    .await
                    .is_some() => {}
                b'i' if self.try_parse_keyword("in", Token::KwIn).await.is_some() => {}
                b'i' if self.try_parse_keyword("if", Token::KwIf).await.is_some() => {}
                b'l' if self.try_parse_keyword("let", Token::KwLet).await.is_some() => {}
                b'r' if self.try_parse_keyword("rec", Token::KwRec).await.is_some() => {}
                b'a' if self
                    .try_parse_keyword("assert", Token::KwAssert)
                    .await
                    .is_some() => {}
                b't' if self
                    .try_parse_keyword("then", Token::KwThen)
                    .await
                    .is_some() => {}
                b'e' if self
                    .try_parse_keyword("else", Token::KwElse)
                    .await
                    .is_some() => {}
                b'w' if self
                    .try_parse_keyword("with", Token::KwWith)
                    .await
                    .is_some() => {}
                b'\'' if self.input.get(1) == Some(b'\'') => {
                    self.lex_indented_string().await?;
                }
                b'$' if self.input.get(1) == Some(b'{') => {
                    self.push(Token::StringBegin).await;
                    self.push(Token::BeginInterpol).await;
                    self.push_brace(BraceStackEntry::UnquotedInterpolation)?;
                    self.input.consume(2);
                }
                b'0'..=b'9' => {
                    self.lex_number().await?;
                }
                ident_pat!(strip_minus) => self.lex_ident().await?,
                c => {
                    return Err(LexError::InvalidChar {
                        char: c,
                        pos: self.input.pos(),
                    });
                }
            }
        }
        self.push(Token::EOF).await;
        Ok(())
    }

    async fn lex_lt_le_or_searchpath(&mut self) {
        // this one may either start a search path, a lone < or a <=.
        let remaining = self.input.slice();
        if let Some(b"<=") = remaining.get(0..2) {
            self.push(Token::Le).await;
            self.input.consume(2);
            return;
        }

        fn match_searchpath(input: &[u8]) -> Option<&str> {
            let end_idx = memchr::memchr(b'>', input)?;
            let candidate_contents = &input[1..end_idx];

            if candidate_contents.is_empty() {
                return None;
            }

            if candidate_contents
                .iter()
                .copied()
                .all(|c| matches!(c, b'A'..=b'z' | b'0'..=b'9' | b'-' | b'/' | b'.' | b'+'))
            {
                core::str::from_utf8(candidate_contents).ok()
            } else {
                None
            }
        }

        if let Some(searchpath) = match_searchpath(remaining) {
            self.push(Token::SearchPath(searchpath)).await;
            self.input.consume(searchpath.len() + 2);
        } else {
            self.push(Token::Lt).await;
            self.input.consume(1);
        }
    }

    async fn lex_number(&mut self) -> Result<(), LexError> {
        let input_slice = self.input.slice();

        let digits_count = input_slice
            .iter()
            .take_while(|n| matches!(n, b'0'..=b'9'))
            .count();
        if self.input.get(digits_count) == Some(b'.') {
            // this is a float!
            let second_part_digits = input_slice[digits_count + 1..]
                .iter()
                .take_while(|n| matches!(n, b'0'..=b'9'))
                .count();
            let total_chars = digits_count + 1 + second_part_digits;
            let chars = self.input.consume(total_chars);
            let float = core::str::from_utf8(chars)
                .map_err(LexError::InvalidString)?
                .parse()
                .map_err(|_| LexError::InvalidFloat)?;
            self.push(Token::Float(float)).await;
        } else {
            // this is just a plain integer
            let chars = self.input.consume(digits_count);

            let int = core::str::from_utf8(chars)
                .map_err(LexError::InvalidString)?
                .parse()
                .map_err(|_| LexError::InvalidInt)?;
            self.push(Token::Integer(int)).await;
        }
        Ok(())
    }

    async fn lex_ident(&mut self) -> Result<(), LexError> {
        let input_slice = self.input.slice();
        let end_idx = input_slice
            .iter()
            .enumerate()
            .find(|(_idx, char)| match char {
                ident_pat!() => false,
                _ => true,
            })
            .map(|(end_idx, _)| end_idx)
            .unwrap_or(input_slice.len());

        let pos = self.input.pos();
        let ident_slice = self.input.consume(end_idx);
        let ident = Self::convert_str(ident_slice)?;
        self.push_pos(Token::Ident(ident), pos).await;
        Ok(())
    }

    async fn try_parse_keyword(&mut self, keyword: &'static str, token: Token<'a>) -> Option<()> {
        let kw_len = keyword.len();

        if !self.input.matches(keyword) {
            return None;
        }

        match self.input.get(kw_len) {
            // the keyword is followed by a character that is valid in an ident, so it is not a keyword here
            Some(ident_pat!()) => None,
            _ => {
                // totally a keyword :)
                self.push(token).await;
                self.input.consume(kw_len);
                Some(())
            }
        }
    }

    fn convert_str(content: &[u8]) -> Result<&str, LexError> {
        core::str::from_utf8(content).map_err(LexError::InvalidString)
    }

    async fn lex_path(&mut self) -> Result<(), LexError> {
        self.push(Token::PathBegin).await;
        self.resume_lex_path().await
    }

    async fn resume_lex_path(&mut self) -> Result<(), LexError> {
        let mut end_idx = None;

        let input_slice = self.input.slice();

        for (idx, char) in input_slice.iter().cloned().enumerate() {
            match char {
                ident_pat!() | b'/' | b'.' | b'+' => {}
                b'$' if matches!(self.input.get(idx + 1), Some(b'{')) => {
                    let pos = self.input.pos();
                    let str = self.input.consume(idx);
                    self.push_string_content(str, pos, false).await?;
                    self.push(Token::BeginInterpol).await;
                    self.input.consume(2);
                    self.push_brace(BraceStackEntry::PathInterpolation)?;
                    return Ok(());
                }
                _ => {
                    end_idx = Some(idx);
                    break;
                }
            }
        }

        let end_idx = end_idx.unwrap_or_else(|| self.input.slice().len());

        let pos = self.input.pos();
        let final_part = self.input.consume(end_idx);
        self.push_string_content(final_part, pos, false).await?;
        self.push(Token::PathEnd).await;
        Ok(())
    }

    async fn push_string_content(
        &mut self,
        mut content: &'a [u8],
        pos: SourcePosition,
        is_multiline_part: bool,
    ) -> Result<(), LexError> {
        fn get_replacement_value(char: &u8) -> Option<&'static str> {
            let res = match char {
                b'n' | b'\n' => "\n",
                b'r' => "\r",
                b'\\' => "\\",
                b'$' => "$",
                _ => return None,
            };
            Some(res)
        }

        let escape_sequence = if is_multiline_part { "''\\" } else { "\\" };
        let finder = memchr::memmem::Finder::new(escape_sequence);

        'outer: loop {
            for sequence_pos in finder.find_iter(content) {
                if let Some(replacement_value) = content
                    .get(sequence_pos + escape_sequence.len())
                    .and_then(get_replacement_value)
                {
                    let unescaped_part = &content[..sequence_pos];
                    if unescaped_part.len() > 0 {
                        self.push_pos(
                            Token::StringContent(Self::convert_str(unescaped_part)?),
                            pos,
                        )
                        .await;
                    }
                    self.push_pos(Token::StringContent(replacement_value), pos)
                        .await;

                    // skip the leading part, including the escape sequence and escaped char
                    content = &content[(sequence_pos + escape_sequence.len() + 1)..];

                    // continue the search on whatever we have not processed yet.
                    continue 'outer;
                }
            }
            break;
        }

        let content = Self::convert_str(content)?;
        if !content.is_empty() {
            self.push(Token::StringContent(content)).await;
        }
        Ok(())
    }

    async fn lex_simple_string(&mut self) -> Result<(), LexError> {
        // skip the starting quotation mark
        self.push(Token::StringBegin).await;
        self.input.advance_one();
        self.resume_lex_simple_string().await
    }

    async fn lex_indented_string(&mut self) -> Result<(), LexError> {
        self.push(Token::IndentedStringBegin).await;
        // skip starting quot
        self.input.consume(2);

        // we need to skip all leading whitespace of the first line if it is actually
        // a multiline string
        let num_leading_spaces = self
            .input
            .slice()
            .iter()
            .take_while(|c| matches!(**c, whitespace_pat!(no_newline)))
            .count();
        if self.input.get(num_leading_spaces) == Some(b'\n') {
            // it is a multiline string!
            self.input.consume(num_leading_spaces + 1);
        }

        self.resume_lex_multiline_string().await
    }

    async fn resume_lex_multiline_string(&mut self) -> Result<(), LexError> {
        'outer: loop {
            let input_slice = self.input.slice();
            let mut search_idx = 0;
            while let Some(idx) = memchr::memchr3(b'\n', b'$', b'\'', &input_slice[search_idx..]) {
                let decide_idx = search_idx + idx;
                let pos = self.input.pos();
                match input_slice[decide_idx] {
                    b'\n' => {
                        let part = self.input.consume(decide_idx + 1);
                        self.push_string_content(part, pos, true).await?;
                        continue 'outer;
                    }
                    b'$' if self.input.get(decide_idx + 1) == Some(b'{') => {
                        let part = self.input.consume(decide_idx);
                        if part.len() > 0 {
                            self.push_string_content(part, pos, true).await?;
                        }
                        // skip the opening dollar brace
                        self.input.consume(2);
                        self.push(Token::BeginInterpol).await;
                        self.push_brace(BraceStackEntry::MultilineStringInterpolation)?;
                        return Ok(());
                    }
                    b'\'' if self.input.get(decide_idx + 1) == Some(b'\'') => {
                        match self.input.get(decide_idx + 2) {
                            Some(b'$' | b'\\') => {
                                // we found something that is escaped
                                search_idx = decide_idx + 3;
                            }
                            _ => {
                                // end of string
                                let part = self.input.consume(decide_idx);
                                if part.len() > 0 {
                                    self.push_string_content(part, pos, true).await?;
                                }
                                self.push(Token::StringEnd).await;

                                // skip the string close quotes
                                self.input.consume(2);

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
        let input_slice = self.input.slice();
        for quot_pos in memchr::memchr_iter(b'"', input_slice) {
            if quot_pos == 0 {
                // this is actually an empty string :)
                self.push(Token::StringEnd).await;
                self.input.advance_one();
                return Ok(());
            }

            // we found a quotation mark.
            let candidate = &input_slice[..quot_pos];

            if ends_with_unsecaped_backslash(candidate) {
                continue;
            }

            for idx in memmem::find_iter(candidate, b"${") {
                let part_to_beginning_of_interpolation = &candidate[..idx];

                if !ends_with_unsecaped_backslash(part_to_beginning_of_interpolation) {
                    // we actually found an interpolation!
                    let pos = self.input.pos();
                    let cont = self.input.consume(part_to_beginning_of_interpolation.len());
                    if cont.len() > 0 {
                        self.push_string_content(cont, pos, false).await?;
                    }
                    self.push_pos(Token::BeginInterpol, pos).await;
                    // skip interpol marker
                    self.input.consume(2);
                    self.push_brace(BraceStackEntry::SimpleStringInterpolation)?;
                    return Ok(());
                }
            }

            // no interpolation found.
            let pos = self.input.pos();
            let cont = self.input.consume(candidate.len());
            self.push_string_content(cont, pos, false).await?;
            self.push(Token::StringEnd).await;
            self.input.advance_one();
            return Ok(());
        }

        Err(LexError::UnclosedString)
    }

    async fn skip_whitespace(&mut self) {
        if let Some(whitespace_pat!()) = self.input.get(0) {
            self.push(Token::Whitespace).await;
            self.input.advance_one();
        } else {
            return;
        }

        while let Some(whitespace_pat!()) = self.input.get(0) {
            self.input.advance_one();
        }
    }

    fn skip_comment(&mut self) {
        if let Some(endline_pos) = memchr::memchr(b'\n', self.input.slice()) {
            self.input.consume(endline_pos + 1);
        } else {
            self.input.consume(self.input.slice().len());
        }
    }

    fn skip_multiline_comment(&mut self) {
        let slice = self.input.slice();
        if let Some(endline_pos) = memmem::find(slice, b"*/") {
            self.input.consume(endline_pos + 2);
        } else {
            self.input.consume(slice.len());
        }
    }
}

pub fn run<'a, TRes>(
    input: &'a [u8],
    consumer: impl FnOnce(IteratorAdapter<'_, '_, TokenWithPosition<'a>, Result<(), LexError>>) -> TRes,
) -> Result<TRes, (LexError, TRes)> {
    let adapter = ImpedanceMatcher::new();

    let future = Lexer {
        input: LexerInput::new(input),
        matcher: &adapter,
        brace_stack: Stack::new(),
        last_was_whitespace: false,
    }
    .run();

    let (lex_res, consumer_res) = adapter.run(future, consumer);

    match lex_res {
        Some(Ok(_)) => Ok(consumer_res),
        Some(Err(e)) => Err((e, consumer_res)),
        None => Err((LexError::NotRunToCompletion, consumer_res)),
    }
}
