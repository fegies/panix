use gc::with_gc;

use crate::{compile_source, vm::value::NixValue, Evaluator};

fn eval_expr(source: &str, expected_source: &str) {
    with_gc(|gc_handle| {
        let full_expr = format!("({source}) == ({expected_source})");
        println!("Evaluating: {full_expr:?}");
        let thunk = compile_source(gc_handle, full_expr.as_bytes()).unwrap();

        let eval_value = Evaluator::new(gc_handle).eval_expression(thunk).unwrap();
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
}

#[test]
fn test_hasattr_multi() {
    eval_expr("{a = 42;} ? a.b", "false");
    eval_expr("{a = {b = 1;};} ? a.b", "true");
    eval_expr("{a = {b = 1;};} ? a.b.c", "false");
    eval_expr("{a = {b = 1;};} ? a.c.c", "false");
}

// #[test]
// fn test_attrset_lazy_resolution() {
//     eval_expr("with {u = 42;}; {a = d; b = u;}.b", "42");
// }

#[test]
fn test_recursive_attrset() {
    eval_expr("let a = 42; in rec {b = a; a = 13;}.b", "13");
}

#[test]
fn test_inherit_from_attrset_to_attrset() {
    eval_expr("let a = {f = 42;}; in {inherit (a) f;}.f", "42");
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
    eval_expr("let f = a : b : a + b; in f 1 4", "5");
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
