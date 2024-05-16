use crate::SourcePosition;

pub struct LexerInput<'input> {
    offset: usize,
    current_position: SourcePosition,
    slice: &'input [u8],
}

impl<'input> LexerInput<'input> {
    pub fn new(slice: &'input [u8]) -> Self {
        Self {
            slice,
            current_position: SourcePosition { line: 1, column: 1 },
            offset: 0,
        }
    }

    pub fn pos(&self) -> SourcePosition {
        self.current_position
    }

    pub fn consume(&mut self, count: usize) -> &'input [u8] {
        let part = &self.slice[self.offset..self.offset + count];
        self.offset += part.len();

        match memchr::memchr_iter(b'\n', part).enumerate().last() {
            Some((num_newlines, last_idx)) => {
                let num_newlines = num_newlines as u32 + 1;
                self.current_position.line += num_newlines;
                self.current_position.column = (part.len() - last_idx) as u32;
            }
            None => {
                // no newline.
                self.current_position.column += count as u32;
            }
        }

        part
    }

    pub fn advance_one(&mut self) {
        if let Some(char) = self.slice.get(self.offset) {
            if *char == b'\n' {
                self.current_position.line += 1;
                self.current_position.column = 0;
            } else {
                self.current_position.column += 1;
            }
        }
        self.offset += 1;
    }

    pub fn get(&self, relative_index: usize) -> Option<u8> {
        self.slice.get(self.offset + relative_index).copied()
    }

    pub fn matches(&self, str: &str) -> bool {
        let str = str.as_bytes();
        let found = self.slice.get(self.offset..(self.offset + str.len()));

        found.map(|found| found == str).unwrap_or(false)
    }

    pub(crate) fn slice(&self) -> &[u8] {
        &self.slice[self.offset..]
    }
}
