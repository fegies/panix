use super::*;

fn lex_test(input: &str, expected: Result<&[Token<'_>], LexError>) {
    let res = run(input.as_bytes(), |adapter| adapter.collect::<Vec<_>>());

    let res = res.as_ref().map_err(|e| e.clone()).map(|v| v.as_ref());
    assert_eq!(expected, res);
}

fn assert_lex(input: &str, expected: &[Token<'_>]) {
    lex_test(input, Ok(expected))
}

#[test]
fn multiline_string() {
    let str = "''''";
    assert_lex(
        str,
        &[Token::IndentedStringBegin, Token::StringEnd, Token::EOF],
    );

    let str = r#"
        ''
        ''
    "#;
    assert_lex(
        str,
        &[
            Token::IndentedStringBegin,
            Token::StringContent("        "),
            Token::StringEnd,
            Token::EOF,
        ],
    );
}
#[test]
fn multiline_string_complex() {
    let str = r#"
    abc = ''
        $foo
        ${bar}
        foo bar
    '';
    "#;
    assert_lex(
        str,
        &[
            Token::Ident("abc"),
            Token::Eq,
            Token::IndentedStringBegin,
            Token::StringContent("        $foo\n"),
            Token::StringContent("        "),
            Token::BeginInterpol,
            Token::Ident("bar"),
            Token::EndInterpol,
            Token::StringContent("\n"),
            Token::StringContent("        foo bar\n"),
            Token::StringContent("    "),
            Token::StringEnd,
            Token::Semicolon,
            Token::EOF,
        ],
    )
}
