use std::collections::HashMap;

use super::*;

enum AttrValue<'a> {
    Expr(NixExpr<'a>),
    Nested(HashMap<&'a str, AttrValue<'a>>),
}

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse an attribute set.
    /// Assumes the initial opening brace and equality sign have been consumed
    pub fn parse_attrset(&mut self, initial_ident: &'t str) -> ParseResult<Attrset<'t>> {
        self.parse_attrset_inner(vec![initial_ident])
    }

    /// parse an attribute set.
    /// Assumes the initial opening brace, and the first ident including a dot have been consumed.
    pub fn parse_attrset_multipath(&mut self, initial_ident: &'t str) -> ParseResult<Attrset<'t>> {
        let mut pieces = vec![initial_ident];
        self.parse_attrset_path(&mut pieces)?;
        self.parse_attrset_inner(pieces)
    }

    /// parse an attribute set.
    /// Assumes the initial opening brace and an inherit keyword have been consumed.
    pub fn parse_attrset_inherit(&mut self) {
        todo!()
    }

    /// fill the provided buffer with pieces of the attribute path.
    /// assumes that a complete ident follows.
    /// (not leading with a dot)
    /// additionally consumed the equality sign.
    fn parse_attrset_path(&mut self, pieces: &mut Vec<&'t str>) -> ParseResult<()> {
        loop {
            pieces.push(self.expect_ident()?);
            match self.expect_next()? {
                Token::Eq => break,
                Token::Dot => {}
                t => unexpected(t)?,
            }
        }
        Ok(())
    }

    fn parse_attrset_path_or_inherit(&mut self, pieces: &mut Vec<&'t str>) -> ParseResult<()> {
        if let Token::KwInherit = self.expect_peek()? {
            todo!()
        }
        self.parse_attrset_path(pieces)
    }

    /// parse an attribute set.
    /// Assumes the initial opening brace and equality sign have already been consumed
    fn parse_attrset_inner(&mut self, mut ident_buf: Vec<&'t str>) -> ParseResult<Attrset<'t>> {
        let initial_value = self.parse_expr()?;
        self.expect(Token::Semicolon)?;

        let mut attrs = HashMap::new();
        insert_nested_attrs(&mut attrs, &ident_buf, initial_value)?;

        loop {
            if let Token::CurlyClose = self.expect_peek()? {
                self.expect_next()?;
                break;
            }

            ident_buf.clear();
            self.parse_attrset_path_or_inherit(&mut ident_buf)?;
            let value: NixExpr<'_> = self.parse_expr()?;
            self.expect(Token::Semicolon)?;

            insert_nested_attrs(&mut attrs, &ident_buf, value)?;
        }

        Ok(Attrset {
            is_recursive: false,
            attrs: convert_nested_attrs(attrs),
        })
    }
}

fn convert_nested_attrs<'t>(
    attrs: HashMap<&'t str, AttrValue<'t>>,
) -> HashMap<&'t str, NixExpr<'t>> {
    attrs
        .into_iter()
        .map(|(k, v)| match v {
            AttrValue::Expr(e) => (k, e),
            AttrValue::Nested(n) => {
                let attrset = Attrset {
                    is_recursive: false,
                    attrs: convert_nested_attrs(n),
                };
                let expr = NixExpr::CompoundValue(CompoundValue::Attrset(attrset));
                (k, expr)
            }
        })
        .collect()
}

fn insert_nested_attrs<'t>(
    mut attrs: &mut HashMap<&'t str, AttrValue<'t>>,
    path: &[&'t str],
    value: NixExpr<'t>,
) -> ParseResult<()> {
    let (last, pieces) = path
        .split_last()
        .expect("should not be called with an empty slice");

    fn conflict(pieces: &[&str], last: &str) -> ParseError {
        ParseError::AttributePathConflict(format!("{pieces:?}, {last:?}"))
    }

    for segment in pieces {
        match attrs
            .entry(*segment)
            .or_insert_with(|| AttrValue::Nested(HashMap::new()))
        {
            AttrValue::Expr(_) => return Err(conflict(pieces, last)),
            AttrValue::Nested(n) => attrs = n,
        }
    }

    if let Some(_) = attrs.insert(last, AttrValue::Expr(value)) {
        return Err(conflict(pieces, last));
    }

    Ok(())
}
