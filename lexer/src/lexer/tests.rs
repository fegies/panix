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
    assert_lex("\"\"", &[Token::StringBegin, Token::StringEnd]);
    assert_lex(
        "\"foo\"",
        &[
            Token::StringBegin,
            Token::StringContent("foo"),
            Token::StringEnd,
        ],
    );
    assert_lex(
        "\" ${foo}\"",
        &[
            Token::StringBegin,
            Token::StringContent(" "),
            Token::BeginInterpol,
            Token::Ident("foo"),
            Token::EndInterpol,
            Token::StringEnd,
        ],
    )
}

#[test]
fn simple_string_with_immediate_interpol() {
    assert_lex(
        "\"${foo}\"",
        &[
            Token::StringBegin,
            Token::BeginInterpol,
            Token::Ident("foo"),
            Token::EndInterpol,
            Token::StringEnd,
        ],
    );
}

#[test]
fn multiline_string_trivial() {
    assert_lex("''''", &[Token::IndentedStringBegin, Token::StringEnd]);

    assert_lex(
        "''foo''",
        &[
            Token::IndentedStringBegin,
            Token::StringContent("foo"),
            Token::StringEnd,
        ],
    )
}
#[test]
fn multiline_interpolated() {
    assert_lex(
        "''${foo}''",
        &[
            Token::IndentedStringBegin,
            Token::BeginInterpol,
            Token::Ident("foo"),
            Token::EndInterpol,
            Token::StringEnd,
        ],
    )
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

#[test]
fn path_with_doubledot() {
    let str = r#"
    ../secrets/cloudflare_dns.age;
    "#;
    assert_lex(
        str,
        &[
            Token::Whitespace,
            Token::PathBegin,
            Token::StringContent("../secrets/cloudflare_dns.age"),
            Token::PathEnd,
            Token::Semicolon,
            Token::Whitespace,
        ],
    )
}

#[test]
fn string_with_escaped_dollar() {
    let str = r#"
    "\${${name}}"
    "#;
    assert_lex(
        str,
        &[
            Token::Whitespace,
            Token::StringBegin,
            Token::StringContent("$"),
            Token::StringContent("{"),
            Token::BeginInterpol,
            Token::Ident("name"),
            Token::EndInterpol,
            Token::StringContent("}"),
            Token::StringEnd,
            Token::Whitespace,
        ],
    );
}

#[test]
fn test_multiline_comment() {
    let str = r#"
    /**
     * foo
     */42"#;
    assert_lex(str, &[Token::Whitespace, Token::Integer(42)]);
}

#[test]
fn multiline_escaped_newline() {
    let str = r#"
        ''
        foo \
        bar
        ''
    "#;
    assert_lex(
        str,
        &[
            Token::Whitespace,
            Token::IndentedStringBegin,
            Token::StringContent("        foo \\\n"),
            Token::StringContent("        bar\n"),
            Token::StringContent("        "),
            Token::StringEnd,
            Token::Whitespace,
        ],
    )
}

#[test]
fn unquoted_iterpolation() {
    let str = "${foo}";
    assert_lex(
        &str,
        &[
            Token::StringBegin,
            Token::BeginInterpol,
            Token::Ident("foo"),
            Token::EndInterpol,
            Token::StringEnd,
        ],
    )
}

#[test]
fn lt_le() {
    assert_lex("< =", &[Token::Lt, Token::Whitespace, Token::Eq]);
    assert_lex("<=", &[Token::Le]);
}

#[test]
fn search_path() {
    assert_lex("<foobar>", &[Token::SearchPath("foobar")]);
    assert_lex("<foo/bar.baz>", &[Token::SearchPath("foo/bar.baz")]);
    assert_lex(" <64>", &[Token::Whitespace, Token::SearchPath("64")]);
    assert_lex(
        "< foo>",
        &[Token::Lt, Token::Whitespace, Token::Ident("foo"), Token::Gt],
    );
    assert_lex(
        "<foo >",
        &[Token::Lt, Token::Ident("foo"), Token::Whitespace, Token::Gt],
    );
    assert_lex("<!>", &[Token::Lt, Token::Not, Token::Gt]);
    assert_lex("<>", &[Token::Lt, Token::Gt]);
}

#[test]
fn ident_with_single_quot() {
    let str = "foo' bar";
    assert_lex(
        &str,
        &[Token::Ident("foo'"), Token::Whitespace, Token::Ident("bar")],
    );
}

#[test]
fn quoted_newline() {
    let str = r#""a\
b""#;
    assert_lex(
        str,
        &[
            Token::StringBegin,
            Token::StringContent("a"),
            Token::StringContent("\nb"),
            Token::StringEnd,
        ],
    );
}
