use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse an interpolation.
    /// This method assumes that the beginInterpolation token has already been consumed.
    fn parse_interpolated_string(
        &mut self,
        initial_part: NixString<'t>,
        end_token: Token<'static>,
    ) -> ParseResult<NixString<'t>> {
        let mut parts = match initial_part.content {
            NixStringContent::Known(KnownNixStringContent::Empty) => Vec::new(),
            NixStringContent::Known(KnownNixStringContent::Literal(lit)) => {
                vec![InterpolationEntry::LiteralPiece(lit)]
            }
            NixStringContent::Known(KnownNixStringContent::Composite(p)) => p
                .into_iter()
                .map(InterpolationEntry::LiteralPiece)
                .collect(),
            NixStringContent::Interpolated(p) => p,
        };

        let expression = self.parse_expr()?;
        parts.push(InterpolationEntry::Expression(expression));
        self.expect(Token::EndInterpol)?;
        loop {
            let t = self.expect_next()?;
            match t.token {
                Token::StringContent(str) => {
                    parts.push(InterpolationEntry::LiteralPiece(str));
                }
                Token::BeginInterpol => {
                    let expr = self.parse_expr()?;
                    parts.push(InterpolationEntry::Expression(expr));
                    self.expect(Token::EndInterpol)?;
                }
                t if t == end_token => {
                    return Ok(NixString {
                        position: initial_part.position,
                        content: NixStringContent::Interpolated(parts),
                    });
                }
                _ => return unexpected(t),
            }
        }
    }

    fn parse_string_content(&mut self, end_token: Token<'static>) -> ParseResult<NixString<'t>> {
        let t = self.expect_next()?;
        let initial_content = match t.token {
            Token::StringContent(cont) => cont,
            Token::BeginInterpol => {
                return self.parse_interpolated_string(
                    NixString {
                        position: t.position,
                        content: NixStringContent::Known(KnownNixStringContent::Empty),
                    },
                    end_token,
                );
            }
            tok if tok == end_token => {
                return Ok(NixString {
                    position: t.position,
                    content: NixStringContent::Known(KnownNixStringContent::Empty),
                })
            }
            _ => return unexpected(t),
        };
        let t = self.expect_next()?;
        let next_part = match t.token {
            Token::StringContent(cont) => cont,
            Token::BeginInterpol => {
                return self.parse_interpolated_string(
                    NixString::from_literal(initial_content, t.position),
                    end_token,
                );
            }
            tok if tok == end_token => {
                return Ok(NixString::from_literal(initial_content, t.position))
            }
            _ => return unexpected(t),
        };
        let mut parts = vec![initial_content, next_part];

        loop {
            let t = self.expect_next()?;
            match t.token {
                Token::StringContent(cont) => {
                    parts.push(cont);
                }
                Token::BeginInterpol => {
                    return self.parse_interpolated_string(
                        NixString {
                            position: t.position,
                            content: NixStringContent::Known(KnownNixStringContent::Composite(
                                parts,
                            )),
                        },
                        end_token,
                    );
                }
                tok if tok == end_token => {
                    return Ok(NixString {
                        position: t.position,
                        content: NixStringContent::Known(KnownNixStringContent::Composite(parts)),
                    })
                }
                _ => return unexpected(t),
            }
        }
    }

    /// parse a simple string.
    /// Assumes the string begin token has already been consumed.
    pub fn parse_simple_string(&mut self) -> ParseResult<NixString<'t>> {
        self.parse_string_content(Token::StringEnd)
    }
    /// parse a path expression.
    /// assumes the path begin token has already been consumed.
    pub fn parse_path(&mut self) -> ParseResult<NixString<'t>> {
        self.parse_string_content(Token::PathEnd)
    }

    /// parse a multiline string. Assumes the string begin token
    /// has already been consumed
    pub fn parse_multiline_string(&mut self) -> ParseResult<NixString<'t>> {
        let mut str = self.parse_string_content(Token::StringEnd)?;

        // we have the string parsed. Now we still need to look at the individual
        // lines inside and strip the appropriate amount of whitespace from it.

        fn count_spaces(s: &str) -> usize {
            s.as_bytes()
                .iter()
                .take_while(|c| matches!(c, b' ' | b'\t' | b'\r'))
                .count()
        }
        fn ends_with_space(s: &str) -> bool {
            if s.is_empty() {
                false
            } else {
                s.as_bytes()[s.len() - 1] == b'\n'
            }
        }
        fn min_spaces<'s>(s: impl Iterator<Item = &'s str>) -> usize {
            let mut counts = true;
            s.flat_map(|s| {
                let res = if counts { Some(count_spaces(s)) } else { None };
                counts = ends_with_space(s);
                res
            })
            .min()
            .unwrap_or(0)
        }

        match &mut str.content {
            NixStringContent::Known(KnownNixStringContent::Literal(lit)) => {
                let leading_spaces = count_spaces(lit);
                *lit = &lit[leading_spaces..];
            }
            NixStringContent::Known(KnownNixStringContent::Composite(comp)) => {
                let spaces = min_spaces(comp.iter().cloned());
                if spaces == 0 {
                    return Ok(str);
                }

                let mut trimmable = true;
                for s in comp {
                    if trimmable {
                        *s = &s[spaces..];
                    }
                    trimmable = ends_with_space(s);
                }
            }
            NixStringContent::Interpolated(int) => {
                let spaces = min_spaces(int.iter().map(|e| match e {
                    InterpolationEntry::LiteralPiece(l) => *l,
                    InterpolationEntry::Expression(_) => "",
                }));
                if spaces == 0 {
                    return Ok(str);
                }

                let mut trimmable = true;
                for e in int {
                    match e {
                        InterpolationEntry::LiteralPiece(l) => {
                            if trimmable {
                                *l = &l[spaces..];
                            }
                            trimmable = ends_with_space(l);
                        }
                        InterpolationEntry::Expression(_) => {
                            trimmable = false;
                        }
                    }
                }
            }
            NixStringContent::Known(KnownNixStringContent::Empty) => {}
        }

        Ok(str)
    }
}
