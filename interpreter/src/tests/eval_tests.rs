use gc::with_gc;
use parser::ast::{NixExpr, SourcePosition};

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
}

#[test]
fn test_plain_value() {
    eval_expr("42", " 42 ");
    eval_expr("''foo''", "\"foo\"");
}
