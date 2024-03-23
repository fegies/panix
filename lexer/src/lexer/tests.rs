use super::*;

fn lex_test(input: &str, expected: Result<&[Token<'_>], LexError>) {
    let mut res = run(input.as_bytes(), |adapter| {
        adapter.map(|t| t.token).collect::<Vec<_>>()
    });

    if let Ok(tokens) = &mut res {
        let last = tokens
            .pop()
            .expect("at least the eof token must be present");
        assert_eq!(Token::EOF, last);
    }

    let res = res
        .as_ref()
        .map_err(|e| e.clone())
        .map(|v| v.as_ref())
        .map_err(|(lr, _)| lr);
    assert_eq!(expected, res);
}

fn assert_lex(input: &str, expected: &[Token<'_>]) {
    lex_test(input, Ok(expected))
}

#[test]
fn empty() {
    assert_lex("", &[]);
}

#[test]
fn simple_ident() {
    assert_lex("foo", &[Token::Ident("foo")]);
}

#[test]
fn simple_string() {
    assert_lex("\"\"", &[Token::StringBegin, Token::StringEnd])
}

#[test]
fn multiline_string() {
    let str = "''''";
    assert_lex(str, &[Token::IndentedStringBegin, Token::StringEnd]);

    let str = r#"
        ''
        ''
    "#;
    assert_lex(
        str,
        &[
            Token::Whitespace,
            Token::IndentedStringBegin,
            Token::StringContent("        "),
            Token::StringEnd,
            Token::Whitespace,
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
            Token::Whitespace,
            Token::Ident("abc"),
            Token::Whitespace,
            Token::Eq,
            Token::Whitespace,
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
            Token::Whitespace,
        ],
    )
}
