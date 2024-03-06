use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse a list expression.
    /// assumes the opening square brace has already been consumed
    pub fn parse_list(&mut self) -> ParseResult<List<'t>> {
        let mut entries = Vec::new();
        loop {
            if let Token::SquareClose = self.expect_peek()? {
                self.expect_next()?;
                break;
            }
            let expr = self.parse_expr_no_spaces()?;
            entries.push(expr);
        }

        Ok(List { entries })
    }
}
