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
}

#[test]
fn test_attrset_lazy_resolution() {
    eval_expr("with {}; {a = d; b = 42;}.b", "42");
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
fn test_list_without_spaces() {
    eval_expr("[(1)(2)]", "[1 2]");
}

#[test]
fn test_funccall_without_spaces() {
    eval_expr("let f = a: a+1; in f(42)", "43");
}

#[test]
fn test_simple_let_in() {
    eval_expr("let a = 42; in a", "42");
}
