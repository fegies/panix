use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse an if expression. Assumes the initial if has already been consumed.
    pub fn parse_if(&mut self) -> ParseResult<IfExpr<'t>> {
        let condition = self.parse_expr()?;
        self.expect(Token::KwThen)?;
        let truthy = self.parse_expr()?;
        self.expect(Token::KwElse)?;
        let falsy = self.parse_expr()?;
        Ok(IfExpr {
            condition: Box::new(condition),
            truthy_case: Box::new(truthy),
            falsy_case: Box::new(falsy),
        })
    }
}
