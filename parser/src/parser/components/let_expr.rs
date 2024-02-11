use std::collections::HashMap;

use lexer::Token;

use crate::parser::{
    ast::{LetInExpr, NixExpr},
    ParseResult, TokenSource,
};

use super::{parse_expr, unexpected};

fn parse_binding<'t>(source: &mut impl TokenSource<'t>) -> ParseResult<(&'t str, NixExpr<'t>)> {
    let ident = match source.expect_next()? {
        Token::Ident(ident) => ident,
        t => return unexpected(t),
    };

    source.expect(Token::Eq)?;

    let body = parse_expr(source)?;
    source.expect(Token::Semicolon)?;

    Ok((ident, body))
}

/// parse a let in expression.
pub fn parse_let<'t>(source: &mut impl TokenSource<'t>) -> ParseResult<LetInExpr<'t>> {
    source.expect(Token::KwLet)?;

    let (ident, body) = parse_binding(source)?;
    let mut bindings = HashMap::new();
    bindings.insert(ident, body);

    while let Some(Token::Ident(_)) = source.peek() {
        let (ident, body) = parse_binding(source)?;
        bindings.insert(ident, body);
    }

    source.expect(Token::KwIn)?;

    let body = Box::new(parse_expr(source)?);

    Ok(LetInExpr { bindings, body })
}
