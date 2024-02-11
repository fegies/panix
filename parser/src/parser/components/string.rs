use lexer::Token;

use super::{parse_expr, unexpected};
use crate::parser::{ast::*, ParseResult, TokenSource};

/// parse an interpolation.
/// This method assumes that the beginInterpolation token has already been consumed.
fn parse_interpolated_string<'t>(
    source: &mut impl TokenSource<'t>,
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

    let expression = parse_expr(source)?;
    parts.push(InterpolationEntry::Expression(expression));
    source.expect(Token::EndInterpol)?;
    loop {
        match source.expect_next()? {
            Token::StringContent(str) => {
                parts.push(InterpolationEntry::LiteralPiece(str));
            }
            Token::BeginInterpol => {
                let expr = parse_expr(source)?;
                parts.push(InterpolationEntry::Expression(expr));
                source.expect(Token::EndInterpol)?;
            }
            Token::StringEnd => {
                return Ok(NixString::Interpolated(parts));
            }
            t => return unexpected(t),
        }
    }
}

fn parse_string_content<'t>(source: &mut impl TokenSource<'t>) -> ParseResult<NixString<'t>> {
    let initial_content = match source.expect_next()? {
        Token::StringEnd => return Ok(NixString::Empty),
        Token::StringContent(cont) => cont,
        Token::BeginInterpol => {
            return parse_interpolated_string(source, NixString::Empty);
        }
        t => return unexpected(t),
    };
    let next_part = match source.expect_next()? {
        Token::StringEnd => return Ok(NixString::Literal(initial_content)),
        Token::StringContent(cont) => cont,
        Token::BeginInterpol => {
            return parse_interpolated_string(source, NixString::Literal(initial_content));
        }
        t => return unexpected(t),
    };
    let mut parts = vec![initial_content, next_part];

    loop {
        match source.expect_next()? {
            Token::StringEnd => return Ok(NixString::Composite(parts)),
            Token::StringContent(cont) => {
                parts.push(cont);
            }
            Token::BeginInterpol => {
                return parse_interpolated_string(source, NixString::Composite(parts));
            }
            t => return unexpected(t),
        }
    }
}

pub fn parse_simple_string<'t>(source: &mut impl TokenSource<'t>) -> ParseResult<NixString<'t>> {
    source.expect(Token::StringBegin)?;
    parse_string_content(source)
}

/// parse a multiline string. Assumes the string begin token
/// has already been consumed
pub fn parse_multiline_string<'t>(source: &mut impl TokenSource<'t>) -> ParseResult<NixString<'t>> {
    source.expect(Token::IndentedStringBegin)?;
    let mut str = parse_string_content(source)?;
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
