use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse an attribute set.
    /// Assumes the initial opening brace, first identifier and equality sign have been consumed
    pub fn parse_attrset(&mut self, initial_ident: NixString<'t>) -> ParseResult<Attrset<'t>> {
        self.parse_attrset_inner(vec![initial_ident], Vec::new())
    }

    /// parse an attribute set.
    /// assumes that the initial opening brace has been consumed.
    pub fn parse_attrset_initial(&mut self) -> ParseResult<Attrset<'t>> {
        self.parse_attrset_inner(Vec::new(), Vec::new())
    }

    /// parse an attribute set.
    /// Assumes the initial opening brace, and the first ident including a dot have been consumed.
    pub fn parse_attrset_multipath(
        &mut self,
        initial_ident: NixString<'t>,
    ) -> ParseResult<Attrset<'t>> {
        let mut pieces = vec![initial_ident];
        self.parse_attrset_path(&mut pieces)?;
        self.parse_attrset_inner(pieces, Vec::new())
    }

    /// parse an attribute set.
    /// Assumes the initial opening brace and an inherit keyword have been consumed.
    pub fn parse_attrset_inherit(&mut self) -> ParseResult<Attrset<'t>> {
        let inherit_keys = vec![self.parse_inherit()?];
        self.parse_attrset_inner(Vec::new(), inherit_keys)
    }

    /// parse an attribute set.
    /// Assumes the initial rec keyword has been consumed.
    pub fn parse_attrset_rec(&mut self) -> ParseResult<Attrset<'t>> {
        self.expect(Token::CurlyOpen)?;
        let mut res = self.parse_attrset_inner(Vec::new(), Vec::new())?;
        res.is_recursive = true;
        Ok(res)
    }

    /// fill the provided buffer with pieces of the attribute path.
    /// assumes that a complete ident follows.
    /// (not leading with a dot)
    /// additionally consumed the equality sign.
    fn parse_attrset_path(&mut self, pieces: &mut Vec<NixString<'t>>) -> ParseResult<()> {
        loop {
            // parse part.
            let t = self.expect_next()?;
            let ident = match t.token {
                Token::StringBegin => self.parse_simple_string()?,
                Token::Ident(i) => NixString::from_literal(i, t.position),
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

    /// parse a inherit clause.
    /// Assumes an initial inherit keyword has already been consumed
    fn parse_inherit(&mut self) -> ParseResult<InheritEntry<'t>> {
        let mut entries = Vec::new();

        // either the first value (no source specified) or the source
        let t = self.expect_next()?;
        let source = match t.token {
            Token::Ident(varname) => {
                entries.push(varname);
                None
            }
            Token::RoundOpen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RoundClose)?;
                // we need to ensure that there is at least one ident provided.
                let t = self.expect_next()?;
                if let Token::Ident(varname) = t.token {
                    entries.push(varname);
                } else {
                    unexpected(t)?;
                }
                Some(Box::new(expr))
            }
            _ => unexpected(t)?,
        };

        // and collect the remaining values (if any)
        loop {
            let t = self.expect_next()?;
            match t.token {
                Token::Ident(varname) => {
                    entries.push(varname);
                }
                Token::Semicolon => {
                    break;
                }
                _ => {
                    unexpected(t)?;
                }
            }
        }

        Ok(InheritEntry { source, entries })
    }

    fn parse_attrset_path_or_inherit(
        &mut self,
        pieces: &mut Vec<NixString<'t>>,
        inherit_keys: &mut Vec<InheritEntry<'t>>,
    ) -> ParseResult<bool> {
        if let Token::KwInherit = self.expect_peek()? {
            self.expect_next()?;
            inherit_keys.push(self.parse_inherit()?);
            Ok(false)
        } else {
            self.parse_attrset_path(pieces)?;
            Ok(true)
        }
    }

    /// parse an attribute set.
    /// Assumes the initial opening brace and equality sign have already been consumed
    fn parse_attrset_inner(
        &mut self,
        mut ident_buf: Vec<NixString<'t>>,
        mut inherit_keys: Vec<InheritEntry<'t>>,
    ) -> ParseResult<Attrset<'t>> {
        let mut attrs = Vec::new();

        if !ident_buf.is_empty() {
            let initial_value = self.parse_expr()?;
            self.expect(Token::Semicolon)?;

            attrs.push((convert_to_attrkey(&mut ident_buf), initial_value));
        }

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

                attrs.push((convert_to_attrkey(&mut ident_buf), value));
            }
        }

        Ok(Attrset {
            is_recursive: false,
            attrs,
            inherit_keys,
        })
    }
}

fn convert_to_attrkey<'t>(ident_buf: &mut Vec<NixString<'t>>) -> AttrsetKey<'t> {
    if ident_buf.len() == 1 {
        AttrsetKey::Single(ident_buf.pop().expect("we know it cannot be empty"))
    } else {
        AttrsetKey::Multi(core::mem::take(ident_buf))
    }
}
