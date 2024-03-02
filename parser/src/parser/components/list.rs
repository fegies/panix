use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse a list expression.
    /// assumes the opening square brace has already been consumed
    pub fn parse_list(&mut self) -> ParseResult<List<'t>> {
        // special fast-path case for the empty list
        if let Token::SquareClose = self.expect_peek()? {
            self.expect_next()?;
            return Ok(List {
                entries: Vec::new(),
            });
        }

        todo!()
    }
}
