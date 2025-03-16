use bumpalo::Bump;
use gc::{GcError, GcHandle, GcPointer};

use crate::{
    EvaluateError,
    vm::value::{self, Attrset, NixString, NixValue, Thunk},
};

#[derive(thiserror::Error, Debug)]
pub enum JsonParseError {
    #[error("Error allocating")]
    GcError(#[from] GcError),
    #[error("Found unexpected trailing data")]
    UnexpectedTrailingData,
    #[error("Unexpected end of data")]
    UnexpectedEof,
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(u8),
    #[error("Found duplicate object keys")]
    DuplicateObjectKeys,
    #[error("could not convert to str")]
    Utf8Error(#[from] core::str::Utf8Error),
    #[error("found an invalid escape sequence")]
    InvalidEscapeSequence,
}

pub struct JsonParser<'gc, 'input> {
    input: &'input [u8],
    gc: &'gc mut GcHandle,
}

pub type ParseResult<T> = Result<T, JsonParseError>;

impl<'input, 'gc> JsonParser<'gc, 'input> {
    pub fn new(input: &'input [u8], gc: &'gc mut GcHandle) -> Self {
        Self { input, gc }
    }

    pub fn parse_json(mut self) -> ParseResult<NixValue> {
        let result = self.parse_json_value()?;
        self.skip_whitespace();
        if self.input.is_empty() {
            Ok(result)
        } else {
            Err(JsonParseError::UnexpectedTrailingData)
        }
    }

    fn parse_json_value(&mut self) -> ParseResult<NixValue> {
        self.skip_whitespace();
        match self.get_next()? {
            b'0'..=b'9' | b'-' => self.parse_number(),
            b'n' => self.parse_null(),
            b't' => self.parse_true(),
            b'f' => self.parse_false(),
            b'{' => self.parse_dict(),
            b'[' => self.parse_list(),
            b'"' => Ok(NixValue::String(self.parse_string()?)),
            c => Err(JsonParseError::UnexpectedCharacter(c)),
        }
    }

    fn parse_null(&mut self) -> ParseResult<NixValue> {
        self.assert_and_remove_keyword("null")?;
        Ok(NixValue::Null)
    }

    fn parse_true(&mut self) -> ParseResult<NixValue> {
        self.assert_and_remove_keyword("true")?;
        Ok(NixValue::Bool(true))
    }

    fn parse_false(&mut self) -> ParseResult<NixValue> {
        self.assert_and_remove_keyword("false")?;
        Ok(NixValue::Bool(false))
    }

    fn parse_object_entry(&mut self) -> ParseResult<(NixString, GcPointer<Thunk>)> {
        let key = self.parse_string()?;
        self.skip_whitespace();
        self.expect_char(b':')?;
        let value = self.parse_json_value()?;
        Ok((key, self.gc.alloc(Thunk::Value(value))?))
    }

    fn parse_dict(&mut self) -> ParseResult<NixValue> {
        self.expect_char(b'{')?;
        let mut entries = Vec::new();

        self.skip_whitespace();
        if self.get_next()? != b'}' {
            entries.push(self.parse_object_entry()?);
        }

        loop {
            self.skip_whitespace();

            if self.get_next()? == b'}' {
                self.input = &self.input[1..];
                break;
            }
            self.expect_char(b',')?;
            self.skip_whitespace();
            entries.push(self.parse_object_entry()?)
        }

        let attrset =
            Attrset::build_from_entries(&mut entries, &mut self.gc).map_err(|e| match e {
                EvaluateError::GcError(g) => JsonParseError::GcError(g),
                _ => JsonParseError::DuplicateObjectKeys,
            })?;

        Ok(NixValue::Attrset(attrset))
    }

    fn parse_number(&mut self) -> ParseResult<NixValue> {
        fn find_main_number<'i>(input: &mut &'i [u8]) -> &'i [u8] {
            let digit_count = input
                .iter()
                .take_while(|c| matches!(c, b'0'..=b'9'))
                .count();
            let result = &input[..digit_count];
            *input = &input[digit_count..];
            result
        }
        fn parse_number(input: &[u8]) -> ParseResult<i64> {
            if input.is_empty() {
                return Err(JsonParseError::UnexpectedEof);
            }

            let mut result = 0;
            for digit in input {
                result *= 10;
                result += (*digit - b'0') as i64
            }
            Ok(result)
        }

        let is_negative = if self.get_next()? == b'-' {
            self.input = &self.input[1..];
            true
        } else {
            false
        };

        let mut main_number = parse_number(find_main_number(&mut self.input))?;
        if self.input.first() == Some(&b'.') {
            self.input = &self.input[1..];
            let decimal_part = find_main_number(&mut self.input);
            let parsed_decimal = parse_number(decimal_part)?;
            let mut result = (main_number as f64)
                + (parsed_decimal as f64 / (10i64.pow(decimal_part.len() as u32)) as f64);
            if is_negative {
                result *= -1f64;
            }
            Ok(NixValue::Float(result))
        } else {
            if is_negative {
                main_number *= -1;
            }
            Ok(NixValue::Int(main_number))
        }
    }

    fn parse_list(&mut self) -> ParseResult<NixValue> {
        self.expect_char(b'[')?;

        let mut entries = Vec::new();

        self.skip_whitespace();
        if self.get_next()? != b']' {
            let first_value = self.parse_json_value()?;
            entries.push(self.gc.alloc(Thunk::Value(first_value))?);
        }
        loop {
            self.skip_whitespace();
            match self.get_next()? {
                b']' => {
                    self.input = &self.input[1..];
                    break;
                }
                b',' => {
                    self.input = &self.input[1..];
                }
                c => return Err(JsonParseError::UnexpectedCharacter(c)),
            }

            let value = self.parse_json_value()?;
            entries.push(self.gc.alloc(Thunk::Value(value))?);
        }

        let entries = self.gc.alloc_vec(&mut entries)?;
        Ok(NixValue::List(value::List { entries }))
    }

    fn parse_string(&mut self) -> ParseResult<NixString> {
        self.expect_char(b'"')?;
        let string_len =
            determine_string_length(&self.input).ok_or(JsonParseError::UnexpectedEof)?;
        let string_data = &self.input[..string_len];
        // we want to skip all the string data as well as the final trailing " char.
        self.input = &self.input[(string_len + 1)..];

        if memchr::memchr(b'\\', string_data).is_none() {
            // no escapes.
            let string = core::str::from_utf8(string_data)?;
            let string = self.gc.alloc_string(string)?;
            return Ok(string.into());
        } else {
            // sadly, there are escape sequences in the string.
            // we need to handle those.

            let arena = Bump::new();

            let mut pieces = Vec::new();
            let mut string_data = core::str::from_utf8(string_data)?;
            while let Some(backslash_idx) = memchr::memchr(b'\\', string_data.as_bytes()) {
                let piece_before = &string_data[..backslash_idx];
                if !piece_before.is_empty() {
                    pieces.push(piece_before);
                }

                let (decoded_sequence, rest) =
                    decode_escape_sequence(&string_data[(backslash_idx + 1)..], &arena)
                        .ok_or(JsonParseError::InvalidEscapeSequence)?;
                pieces.push(decoded_sequence);
                string_data = rest;
            }
            // whatever remains now does not have any more escapes and can be included.
            if !string_data.is_empty() {
                pieces.push(string_data);
            }

            let result = self.gc.alloc_string_from_parts(&pieces)?.into();
            return Ok(result);
        }

        // finds the first not escaped " char.
        fn determine_string_length(input: &[u8]) -> Option<usize> {
            for qoute_idx in memchr::memchr_iter(b'"', input) {
                let candidate_substring = &input[..qoute_idx];
                let num_backslashes = candidate_substring
                    .iter()
                    .rev()
                    .take_while(|c| **c == b'\\')
                    .count();
                if num_backslashes % 2 == 0 {
                    return Some(qoute_idx);
                }
            }
            None
        }

        // decode an escape sequence. The initial backslash should not be passed.
        // returns a tuple containing:
        // - the decoded sequence
        // - the remaining input
        //
        // in that order.
        fn decode_escape_sequence<'i, 'a>(
            input: &'i str,
            arena: &'a Bump,
        ) -> Option<(&'a str, &'i str)> {
            let seq = match input.as_bytes().get(0)? {
                b'"' => "\"",
                b'\\' => "\\",
                b'/' => "/",
                b'b' => "\x08",
                b'f' => "\x0c",
                b'n' => "\n",
                b'r' => "\r",
                b't' => "\t",
                b'u' => {
                    let codepoint = input.get(1..5)?;
                    let codepoint = char::from_u32(u32::from_str_radix(codepoint, 16).ok()?)?;

                    let mut buffer = [0; 4];
                    let seq = arena.alloc_str(codepoint.encode_utf8(&mut buffer));

                    return Some((seq, &input[5..]));
                }
                _ => {
                    return None;
                }
            };

            Some((seq, &input[1..]))
        }
    }

    fn expect_char(&mut self, char: u8) -> ParseResult<()> {
        let first = self.input.first().ok_or(JsonParseError::UnexpectedEof)?;
        if *first == char {
            self.input = &self.input[1..];
            Ok(())
        } else {
            Err(JsonParseError::UnexpectedCharacter(*first))
        }
    }

    fn assert_and_remove_keyword(&mut self, keyword: &str) -> ParseResult<()> {
        let kw_len = keyword.len();
        let input = self
            .input
            .get(..kw_len)
            .ok_or(JsonParseError::UnexpectedEof)?;

        if input == keyword.as_bytes() {
            self.input = &self.input[kw_len..];
            Ok(())
        } else {
            Err(JsonParseError::UnexpectedCharacter(input[0]))
        }
    }

    fn get_next(&self) -> ParseResult<u8> {
        self.input
            .first()
            .copied()
            .ok_or(JsonParseError::UnexpectedEof)
    }

    /// skip 0 or more whitespace characters from the input beginning
    fn skip_whitespace(&mut self) {
        let num_whitespace = self
            .input
            .iter()
            .take_while(|c| matches!(c, b' ' | b'\t' | b'\r' | b'\n'))
            .count();

        self.input = &self.input[num_whitespace..];
    }
}
