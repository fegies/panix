use gc::with_gc;

use crate::{compile_source, vm::value::NixValue, Evaluator};

fn eval_expr(source: &str, expected_source: &str) {
    with_gc(|gc_handle| {
        let full_expr = format!("({source}) == ({expected_source})");
        println!("\nEvaluating: {full_expr:?}\n");
        let thunk = compile_source(gc_handle, full_expr.as_bytes(), "<<inline>>").unwrap();

        let eval_value = Evaluator::new(gc_handle)
            .unwrap()
            .eval_expression(thunk)
            .unwrap();
        if let NixValue::Bool(true) = eval_value {
            println!("successfully evaluated to true");
        } else {
            panic!("equality assertion failed, got {eval_value:?}");
        }
    })
    .expect("GC error");
}

#[test]
fn test_basic_arithmentic() {
    eval_expr("1 + 2 * 3", "7");
    eval_expr("6 / 2", "3");
}

#[test]
fn test_eval_divs() {
    eval_expr("1 + (1.0 / 2.0) * (3.0 / 4)", "11.0 / 8");
    eval_expr("-1 + (-1.0 / -2.0) * (-3.0 / -4)", "-5.0 / 8");
}

#[test]
fn test_sub() {
    eval_expr("42 - 11", "30 + 1");
    eval_expr("42 - - 1", "43");
}

#[test]
fn test_braces() {
    eval_expr("4 * (3 + 2)", "20");
}

#[test]
fn test_plain_value() {
    eval_expr("42", " 42 ");
    eval_expr("''foo''", "\"foo\"");
}

#[test]
fn test_if() {
    eval_expr("if 1 == 2 then 3 else 2", "2");
    eval_expr("if 1 == 1 then 1 + 1 else 1", "2");
}

#[test]
fn test_alloc_list() {
    eval_expr("[1 2 3]", "[1   2   3]");
    eval_expr("[] < [1]", "true");
    eval_expr("[] < []", "false");
}

#[test]
fn test_simple_attrset() {
    eval_expr("{}", "{}");
    eval_expr("{a = 1;}", "{ a = 1; }");
    eval_expr("{a = 1;} == {a = 2; }", "false");
}

#[test]
fn test_get_attribute() {
    eval_expr("{a = 42;}.a", "42");
    eval_expr("{a = 1;}.a or 42", "1");
    eval_expr("{a = 1;}.b or 42", "42");
}

#[test]
fn test_hasattr() {
    eval_expr("{a = 42;} ? a", "true");
    eval_expr("{a = 42;} ? b", "false");
    eval_expr("12 ? a", "false");
    eval_expr("let i = \"a\"; in {a = 42;} ? \"${i}\"", "true");
    eval_expr("null ? x", "false");
    eval_expr("null ? x.y", "false");
}

#[test]
fn test_hasattr_multi() {
    eval_expr("{a = 42;} ? a.b", "false");
    eval_expr("{a = {b = 1;};} ? a.b", "true");
    eval_expr("{a = {b = 1;};} ? a.b.c", "false");
    eval_expr("{a = {b = 1;};} ? a.c.c", "false");
}

#[test]
fn test_recursive_attrset() {
    eval_expr("let a = 42; in rec {b = a; a = 13;}.b", "13");
}

#[test]
fn test_inherit_from_attrset_to_attrset() {
    eval_expr("let a = {f = 42;}; in {inherit (a) f;}.f", "42");
}

#[test]
fn test_inherit_from_attrset() {
    eval_expr(
        "let a = {b = 42; c = 14;}; in let inherit(a) b c; in b + c",
        "56",
    );
}

#[test]
fn test_list_without_spaces() {
    eval_expr("[(1)(2)]", "[1 2]");
}

#[test]
fn test_boolean_logic() {
    eval_expr("true || false", "true");
    eval_expr("true && false", "false");
}

#[test]
fn test_funccall_without_spaces() {
    eval_expr("let f = a: a+1; in f(42)", "43");
}

#[test]
fn test_funccall() {
    eval_expr("(let x = 12; in (a : a + x)) 2", "14");
}

#[test]
fn test_currying() {
    eval_expr("let f = a : b : a + b; g = f 1; in g 4", "5");
}

#[test]
fn test_from_arg() {
    eval_expr("let a = 42; in (v: v) a", "42");
}

#[test]
fn test_attrset_lambda() {
    eval_expr("({a, b ? null}: a) {a = 42;}", "42");
    eval_expr("({a, b ? null}: b) {a = 42;}", "null");
    eval_expr("({a, b ? null}: b) {b = 12; a = null;}", "12");
    eval_expr("(arg@{a, ...}: arg ){a = 42; b = 42;}", "{a = 42; b = 42;}");
}

#[test]
fn test_attrset_reference_other_arg() {
    eval_expr("({a, b ? a }: b) {a = 42;}", "42");
}

#[test]
fn test_simple_let_in() {
    eval_expr("let a = 42; b = 11; in a", "42");
}
#[test]
fn test_let_reuse() {
    eval_expr("let a = 1 + 1; in a + a", "4");
}

#[test]
fn test_nested_let() {
    eval_expr("let a = 1; b = let c = 2; in a + c; in b", "3");
    eval_expr("let a = 1; in (let b = 2; in a + b)", "3");
}

#[test]
fn test_with_expr() {
    eval_expr("with {a = 42;}; a + 1", "43");
}

#[test]
fn test_nested_with() {
    eval_expr("with {a = 42;}; with {b = 13;}; a + b", "55");
}

#[test]
fn test_attrset_lazy_resolution() {
    eval_expr("with {u = 42;}; {a = d; b = u;}.b", "42");
}

#[test]
fn test_add_strings() {
    eval_expr("''foo'' + ''bar''", "\"foobar\"");
}

#[test]
fn test_string_interpol() {
    eval_expr(
        "let a = \"42\"; b = \"15\"; in \"foo ${a}${a} bar ${b}\"",
        "\"foo 4242 bar 15\"",
    );
}

#[test]
fn test_tryeval() {
    eval_expr(
        "builtins.tryEval (throw \"foo\")",
        "{success = false; value = false;}",
    );
    eval_expr("builtins.tryEval (42)", "{success = true; value = 42;}");
}

#[test]
fn test_typeof() {
    let pairs = &[
        ("42", "int"),
        ("true", "bool"),
        ("\"foo\"", "string"),
        ("null", "null"),
        ("{}", "set"),
        ("[]", "list"),
        ("./foo", "path"),
        ("42.1", "float"),
        ("a: a", "lambda"),
    ];

    for (nix_expr, type_) in pairs {
        eval_expr(
            &format!("builtins.typeOf {nix_expr}"),
            &(format!("\"{type_}\"")),
        )
    }

    eval_expr("builtins.isAttrs {}", "true");
}

#[test]
fn test_attrset_inherit() {
    eval_expr(
        "(let a = 12; b = 42; f = null; in {inherit a b f; c = 11; \"00\" = 31;}).a",
        "12",
    );
    eval_expr("(let b = 42; in {inherit b; a = 12;}).b", "42");
}

#[test]
fn test_tostring() {
    let pairs = &[
        ("\"foo\"", "foo"),
        ("/foo/bar", "/foo/bar"),
        ("42", "42"),
        ("42.1", "42.1"),
        ("false", ""),
        ("true", "1"),
        ("null", ""),
        ("[42 12 \"foo\"]", "42 12 foo"),
        ("{outPath = 42;}", "42"),
        ("{__toString = self: true;}", "1"),
    ];

    for (nix_expr, str_repr) in pairs {
        eval_expr(
            &format!("builtins.toString {nix_expr}"),
            &format!("\"{str_repr}\""),
        );
        println!("\n---\n");
        // toString should exist in the global scope too
        eval_expr(&format!("toString {nix_expr}"), &format!("\"{str_repr}\""));
        println!("\n---\n");
    }
}

#[test]
fn test_map() {
    eval_expr(
        r#"map (x: "foo" + x) [ "bar" "bla" "abc" ]"#,
        r#"[ "foobar" "foobla" "fooabc" ]"#,
    );
}

#[test]
fn test_split() {
    eval_expr(r#"builtins.split "\\." "1.2.3""#, r#"["1" [] "2" [] "3"]"#);

    println!("\n---\n");

    eval_expr(r#"builtins.split "(a)b" "abc""#, r#"[ "" [ "a" ] "c" ]"#);
    println!("\n---\n");
    eval_expr(
        r#"builtins.split "([ac])" "abc""#,
        r#"[ "" [ "a" ] "b" [ "c" ] "" ]"#,
    );
    println!("\n---\n");
    eval_expr(
        r#"builtins.split "([[:upper:]]+)" " FOO ""#,
        r#"[ " " [ "FOO" ] " " ]"#,
    );
    println!("\n---\n");
    eval_expr(
        r#"builtins.split "(a)|(c)" "abc""#,
        r#"[ "" [ "a" null ] "b" [ null "c" ] "" ]"#,
    );
}

#[test]
fn test_nested_builtin() {
    eval_expr("builtins.builtins.builtins.null", "null");
}

#[test]
fn test_filter() {
    eval_expr(
        "builtins.filter builtins.isInt [ \"foo\" 1 2.2 3 4 null ]",
        "[1 3 4]",
    );
}

#[test]
fn test_split_version() {
    let res = &[
        ("1..", "[\"1\"]"),
        ("1.1", "[\"1\" \"1\"]"),
        ("1..1", "[\"1\" \"1\"]"),
    ];
    for (vers, expected) in res {
        eval_expr(&format!("builtins.splitVersion \"{vers}\""), expected);
    }
}

#[test]
fn test_compare_versions() {
    let exprs = &[
        ("0.0.0", "0.0.0", 0),
        ("0.0.1", "0.0.0", 1),
        ("0p1", "0.0.0", -1),
        ("1.0.54", "0.1.102", 1),
    ];
    for (vers_l, vers_r, res) in exprs {
        eval_expr(
            &format!("builtins.compareVersions \"{vers_l}\" \"{vers_r}\""),
            &format!("{res}"),
        );
        println!("\n---\n");
    }
}

#[test]
fn test_list_len() {
    eval_expr("builtins.length [ 1 null 2 42 ]", "4")
}

#[test]
fn test_elemat() {
    eval_expr("builtins.elemAt [ 1 2 3] 1", "2");
}

#[test]
fn test_cat_attrs() {
    eval_expr(
        "builtins.catAttrs \"a\" [{a = 1;} {b = 0;} {a = 2;}]",
        "[1 2]",
    );
}

#[test]
fn test_concat_lists() {
    eval_expr("builtins.concatLists [ [1] [2] [3] ]", "[1 2 3]");
}

#[test]
fn test_from_json() {
    let res = &[
        ("null", "null"),
        ("42", "42"),
        ("42.42", "42.42"),
        ("{}", "{}"),
        ("[]", "[]"),
        ("[42]", "[42]"),
        ("[42 , 13]", "[42 13]"),
        ("\"foo\"", "\"foo\""),
        (r#""foo \" ""#, r#"''foo " ''"#),
        (r#"{"a" : 42}"#, "{a = 42;}"),
        (r#"{"a": 42, "b": "foo"}"#, "{a = 42; b = \"foo\";}"),
    ];
    for (json, nix) in res {
        eval_expr(&format!("builtins.fromJSON ''{json}''"), nix);
        println!("\n-----\n");
    }
}

#[test]
fn test_partition() {
    eval_expr(
        "builtins.partition (x: x > 10) [1 23 9 3 42]",
        "{ right = [ 23 42 ]; wrong = [ 1 9 3 ]; }",
    );
}

#[test]
fn test_assert() {
    eval_expr("assert 1 == 1; 42", "42");
    eval_expr("(builtins.tryEval (assert 1 == 2; 42)).value", "false");
}

#[test]
fn test_remove_attrs() {
    eval_expr(
        r#"builtins.removeAttrs { x = 1; y = 2; z = 3; } [ "a" "x" "z" ]"#,
        "{y = 2;}",
    );
    // and again, to assert it is present in global scope.
    eval_expr(
        r#"removeAttrs { x = 1; y = 2; z = 3; } [ "a" "x" "z" ]"#,
        "{y = 2;}",
    );
}

#[test]
fn test_inherit_rec() {
    eval_expr(
        "let b = {a = 42;}; in rec {inherit (b) a; c = a;}",
        "{a = 42; c = 42;}",
    );
}

#[test]
fn test_gen_list() {
    eval_expr(
        "builtins.genList (x: x * x) 10",
        "[ 0 1 4 9 16 25 36 49 64 81]",
    );
    eval_expr("builtins.genList (a: a) 0", "[]");
}

#[test]
fn test_basenameof() {
    eval_expr("baseNameOf \"abc/def\"", "\"def\"");
    eval_expr("baseNameOf \"def\"", "\"def\"");
    eval_expr("baseNameOf \"\"", "\"\"");
    eval_expr("baseNameOf \"foo/\"", "\"foo\"");
    eval_expr("baseNameOf \"foo/bar/\"", "\"bar\"");
    eval_expr("baseNameOf \"/\"", "\"\"");

    eval_expr("baseNameOf ./foo", "\"foo\"");
}

#[test]
fn test_match() {
    eval_expr("builtins.match \"ab\" \"abc\"", "null");
    eval_expr("builtins.match \"abc\" \"abc\"", "[]");
    eval_expr("builtins.match \"a(b)(c)\" \"abc\"", "[\"b\" \"c\"]");
    eval_expr(
        r#"builtins.match "[[:space:]]+([[:upper:]]+)[[:space:]]+" "  FOO   ""#,
        "[\"FOO\"]",
    );
}

#[test]
fn test_map_attrs() {
    eval_expr(
        "builtins.mapAttrs (name: value: value * 10) { a = 1; b = 2; }",
        "{ a = 10; b = 20; }",
    );
}

#[test]
fn test_dirof() {
    let pairs = &[
        ("", "."),
        ("foo", "."),
        ("/", "/"),
        ("abc/def", "abc"),
        ("/foo", "/"),
        ("abc/", "abc"),
    ];
    for (input, expected) in pairs {
        eval_expr(&format!("dirOf \"{input}\""), &format!("\"{expected}\""));
        println!("\n\n---\n\n");
    }
}

#[test]
fn test_stringlen() {
    eval_expr("builtins.stringLength \"foo\"", "3");
}

#[test]
fn test_substring() {
    eval_expr(r#"builtins.substring 0 3 "nixos""#, "\"nix\"");
}

#[test]
fn test_attr_values() {
    eval_expr("builtins.attrValues {a = 42; b = 12;}", "[42 12]");
}

#[test]
fn test_replace_strings() {
    eval_expr(
        r#"builtins.replaceStrings ["oo" "a"] ["a" "i"] "foobar""#,
        r#""fabir""#,
    );
    eval_expr(
        r#"let dbg = a: builtins.trace a a; in dbg (builtins.replaceStrings ["a" ""] ["b" "X"] "fafa")"#,
        "\"XfbXfbX\"",
    );
    eval_expr(
        r#"builtins.replaceStrings ["aa" ""] ["b" "X"] "faafa""#,
        "\"XfbXfXaX\"",
    );
    eval_expr(
        r#"builtins.replaceStrings ["aa" ""] ["b" "X"] "faafa""#,
        "\"XfbXfXaX\"",
    );
}

#[test]
fn test_replace_strings_empty() {
    eval_expr(r#"builtins.replaceStrings [""] ["X"] """#, "\"X\"");
}

#[test]
fn test_foldl() {
    eval_expr("builtins.foldl' (x: y: x + y) 0 [1 2 3]", "6");
}

#[test]
fn test_sort() {
    eval_expr(
        "builtins.sort builtins.lessThan [ 483 249 526 147 42 77 ]",
        "[ 42 77 147 249 483 526 ]",
    );
}

#[test]
fn test_inherit_from_nested() {
    eval_expr(
        "(a: let inherit (a.b) d; inherit (a.c) e; in [d e]) {b.d = 42; c.e = 13;}",
        "[42 13]",
    );
}

#[test]
fn eval_long_tailcall() {
    eval_expr(
        r#"
    let huge_list = builtins.genList (a: a) 10000;
    in builtins.foldl' (acc: val: acc + val) 0 huge_list"#,
        "49995000",
    );
}

#[test]
fn test_all() {
    eval_expr("builtins.all (v: false) []", "true");
    eval_expr("builtins.all (x: x < 3) [1 2]", "true");
    eval_expr("builtins.all (x: x < 3) [1 2 3]", "false");
}

#[test]
fn test_any() {
    eval_expr("builtins.any (v: true) []", "false");
    eval_expr("builtins.any (x: x < 3) [5 6]", "false");
    eval_expr("builtins.any (x: x < 3) [2 5 3]", "true");
}

#[test]
fn test_attrnames() {
    eval_expr(
        "builtins.attrNames { y = 1; x = \"foo\"; }",
        "[ \"x\" \"y\" ]",
    );
}

#[test]
fn test_mergeattr() {
    eval_expr("{a = 1; b = 3;} // {a = 2;}", "{b = 3; a = 2;}");
}

#[test]
fn test_trace() {
    eval_expr("builtins.trace 42 1", "1");
}
