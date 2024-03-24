use std::collections::{HashMap, HashSet};

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
    pub fn parse_attrset_inherit(&mut self) -> ParseResult<Attrset<'t>> {
        todo!()
    }

    /// parse an attribute set.
    /// Assumes the initial rec keyword has been consumed.
    pub fn parse_attrset_rec(&mut self) -> ParseResult<Attrset<'t>> {
        todo!()
    }

    /// fill the provided buffer with pieces of the attribute path.
    /// assumes that a complete ident follows.
    /// (not leading with a dot)
    /// additionally consumed the equality sign.
    fn parse_attrset_path(&mut self, pieces: &mut Vec<&'t str>) -> ParseResult<()> {
        loop {
            // parse part.
            let t = self.expect_next()?;
            let ident = match t.token {
                Token::StringBegin => {
                    let str = self.parse_simple_string()?;
                    match str {
                        NixString::Literal(l) => l,
                        NixString::Composite(_) => todo!(),
                        NixString::Interpolated(_) => todo!(),
                        NixString::Empty => "",
                    }
                }
                Token::Ident(i) => i,
                _ => unexpected(t)?,
            };
            pieces.push(ident);

            let t = self.expect_next()?;
            match t.token {
                Token::Eq => break,
                Token::Dot => {}
                _ => unexpected(t)?,
            }
        }
        Ok(())
    }

    fn parse_inherit(&mut self, inherit_keys: &mut HashSet<&'t str>) -> ParseResult<()> {
        loop {
            let t = self.expect_next()?;
            match t.token {
                Token::Ident(varname) => {
                    if !inherit_keys.insert(varname) {
                        return Err(ParseError::AttributePathConflict(varname.to_owned()));
                    }
                }
                Token::Semicolon => return Ok(()),
                _ => unexpected(t)?,
            }
        }
    }

    fn parse_attrset_path_or_inherit(
        &mut self,
        pieces: &mut Vec<&'t str>,
        inherit_keys: &mut HashSet<&'t str>,
    ) -> ParseResult<bool> {
        if let Token::KwInherit = self.expect_peek()? {
            self.expect_next()?;
            self.parse_inherit(inherit_keys)?;
            Ok(false)
        } else {
            self.parse_attrset_path(pieces)?;
            Ok(true)
        }
    }

    /// parse an attribute set.
    /// Assumes the initial opening brace and equality sign have already been consumed
    fn parse_attrset_inner(&mut self, mut ident_buf: Vec<&'t str>) -> ParseResult<Attrset<'t>> {
        let initial_value = self.parse_expr()?;
        self.expect(Token::Semicolon)?;

        let mut attrs = HashMap::new();
        let mut inherit_keys = HashSet::new();
        insert_nested_attrs(&mut attrs, &ident_buf, initial_value)?;

        loop {
            let peeked = self.expect_peek()?;
            if let Token::CurlyClose = peeked {
                self.expect_next()?;
                break;
            }

            ident_buf.clear();
            if self.parse_attrset_path_or_inherit(&mut ident_buf, &mut inherit_keys)? {
                let value: NixExpr<'_> = self.parse_expr()?;
                self.expect(Token::Semicolon)?;

                insert_nested_attrs(&mut attrs, &ident_buf, value)?;
            }
        }

        Ok(Attrset {
            is_recursive: false,
            attrs: convert_nested_attrs(attrs),
            inherit_keys,
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
                    inherit_keys: HashSet::new(),
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

    #[cold]
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
