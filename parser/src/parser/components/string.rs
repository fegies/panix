use super::*;

impl<'t, S: TokenSource<'t>> Parser<S> {
    /// parse an interpolation.
    /// This method assumes that the beginInterpolation token has already been consumed.
    fn parse_interpolated_string(
        &mut self,
        initial_part: NixString<'t>,
    ) -> ParseResult<NixString<'t>> {
        let mut parts = match initial_part {
            NixString::Literal(lit) => vec![InterpolationEntry::LiteralPiece(lit)],
            NixString::Composite(p) => p
                .into_iter()
                .map(InterpolationEntry::LiteralPiece)
                .collect(),
            NixString::Interpolated(p) => p,
            NixString::Empty => Vec::new(),
        };

        let expression = self.parse_expr()?;
        parts.push(InterpolationEntry::Expression(expression));
        self.expect(Token::EndInterpol)?;
        loop {
            match self.expect_next()? {
                Token::StringContent(str) => {
                    parts.push(InterpolationEntry::LiteralPiece(str));
                }
                Token::BeginInterpol => {
                    let expr = self.parse_expr()?;
                    parts.push(InterpolationEntry::Expression(expr));
                    self.expect(Token::EndInterpol)?;
                }
                Token::StringEnd => {
                    return Ok(NixString::Interpolated(parts));
                }
                t => return unexpected(t),
            }
        }
    }

    fn parse_string_content(&mut self) -> ParseResult<NixString<'t>> {
        let initial_content = match self.expect_next()? {
            Token::StringEnd => return Ok(NixString::Empty),
            Token::StringContent(cont) => cont,
            Token::BeginInterpol => {
                return self.parse_interpolated_string(NixString::Empty);
            }
            t => return unexpected(t),
        };
        let next_part = match self.expect_next()? {
            Token::StringEnd => return Ok(NixString::Literal(initial_content)),
            Token::StringContent(cont) => cont,
            Token::BeginInterpol => {
                return self.parse_interpolated_string(NixString::Literal(initial_content));
            }
            t => return unexpected(t),
        };
        let mut parts = vec![initial_content, next_part];

        loop {
            match self.expect_next()? {
                Token::StringEnd => return Ok(NixString::Composite(parts)),
                Token::StringContent(cont) => {
                    parts.push(cont);
                }
                Token::BeginInterpol => {
                    return self.parse_interpolated_string(NixString::Composite(parts));
                }
                t => return unexpected(t),
            }
        }
    }

    /// parse a simple string.
    /// Assumes the string begin token has already been consumed.
    pub fn parse_simple_string(&mut self) -> ParseResult<NixString<'t>> {
        self.parse_string_content()
    }

    /// parse a multiline string. Assumes the string begin token
    /// has already been consumed
    pub fn parse_multiline_string(&mut self) -> ParseResult<NixString<'t>> {
        let mut str = self.parse_string_content()?;

        // we have the string parsed. Now we still need to look at the individual
        // lines inside and strip the appropriate amount of whitespace from it.

        fn count_spaces(s: &str) -> usize {
            s.as_bytes()
                .iter()
                .take_while(|c| matches!(c, b' ' | b'\t' | b'\r'))
                .count()
        }
        fn ends_with_space(s: &str) -> bool {
            s.as_bytes()[s.len() - 1] == b'\n'
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

        match &mut str {
            NixString::Literal(lit) => {
                let leading_spaces = count_spaces(lit);
                *lit = &lit[leading_spaces..];
            }
            NixString::Composite(comp) => {
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
            NixString::Interpolated(int) => {
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
            NixString::Empty => {}
        }

        Ok(str)
    }
}
