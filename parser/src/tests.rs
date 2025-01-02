use std::collections::BTreeMap;

use lexer::SourcePosition;

use crate::{
    ast::{
        Attrset, BasicValue, BinopOpcode, Code, CompoundValue, InheritEntry, Lambda,
        LambdaAttrsetArgs, NixExpr, NixString, Op,
    },
    parse_nix,
};

#[test]
fn test_parse_minus() {
    let value = parse_nix("2 - 1".as_bytes()).unwrap();
    println!("{value:#?}");
    assert_eq!(
        NixExpr {
            position: SourcePosition { line: 1, column: 3 },
            content: crate::ast::NixExprContent::Code(crate::ast::Code::Op(
                crate::ast::Op::Binop {
                    left: Box::new(NixExpr {
                        position: SourcePosition { line: 1, column: 2 },
                        content: crate::ast::NixExprContent::BasicValue(BasicValue::Int(2))
                    }),
                    right: Box::new(NixExpr {
                        position: SourcePosition { line: 1, column: 6 },
                        content: crate::ast::NixExprContent::BasicValue(BasicValue::Int(1))
                    }),
                    opcode: BinopOpcode::Subtract,
                }
            )),
        },
        value
    );
}

#[test]
fn test_parse_attrset_or() {
    let expected = NixExpr {
        position: SourcePosition { line: 1, column: 3 },
        content: crate::ast::NixExprContent::Code(Code::Op(Op::AttrRef {
            left: Box::new(NixExpr {
                position: SourcePosition { line: 1, column: 1 },
                content: crate::ast::NixExprContent::CompoundValue(CompoundValue::Attrset(
                    Attrset::empty(),
                )),
            }),
            name: NixString::from_literal("foo", SourcePosition { line: 1, column: 4 }),
            default: Some(Box::new(NixExpr {
                position: SourcePosition {
                    line: 1,
                    column: 11,
                },
                content: crate::ast::NixExprContent::Code(Code::ValueReference { ident: "false" }),
            })),
        })),
    };
    let value = parse_nix("{}.foo or false".as_bytes()).unwrap();
    assert_eq!(expected, value);
}

#[test]
fn test_parse_attrset_inherit() {
    let expected = NixExpr {
        position: SourcePosition { line: 1, column: 1 },
        content: crate::ast::NixExprContent::CompoundValue(CompoundValue::Attrset(Attrset {
            is_recursive: false,
            inherit_keys: vec![InheritEntry {
                source: Some(Box::new(NixExpr {
                    position: SourcePosition {
                        line: 1,
                        column: 11,
                    },
                    content: crate::ast::NixExprContent::Code(Code::ValueReference { ident: "a" }),
                })),
                entries: vec!["b"],
            }],
            attrs: vec![],
        })),
    };
    let value = parse_nix("{inherit (a) b;}".as_bytes()).unwrap();
    assert_eq!(expected, value);
}

#[test]
fn test_funccall_without_spaces() {
    let expected = NixExpr {
        position: SourcePosition { line: 1, column: 2 },
        content: crate::ast::NixExprContent::Code(Code::Op(Op::Call {
            function: Box::new(NixExpr {
                position: SourcePosition { line: 1, column: 1 },
                content: crate::ast::NixExprContent::Code(Code::ValueReference { ident: "f" }),
            }),
            arg: Box::new(NixExpr {
                position: SourcePosition { line: 1, column: 4 },
                content: crate::ast::NixExprContent::BasicValue(BasicValue::Int(1)),
            }),
        })),
    };
    let value = parse_nix("f(1)".as_bytes()).unwrap();
    assert_eq!(expected, value);
}

#[test]
fn test_list_without_spaces() {
    let expected = NixExpr {
        position: SourcePosition { line: 1, column: 1 },
        content: crate::ast::NixExprContent::CompoundValue(CompoundValue::List(crate::ast::List {
            entries: vec![
                NixExpr {
                    position: SourcePosition { line: 1, column: 4 },
                    content: crate::ast::NixExprContent::BasicValue(BasicValue::Int(1)),
                },
                NixExpr {
                    position: SourcePosition { line: 1, column: 7 },
                    content: crate::ast::NixExprContent::BasicValue(BasicValue::Int(2)),
                },
            ],
        })),
    };
    let value = parse_nix("[(1)(2)]".as_bytes()).unwrap();
    assert_eq!(expected, value);
}

#[test]
fn test_search_path() {
    let expected = NixExpr {
        position: SourcePosition { line: 1, column: 1 },
        content: crate::ast::NixExprContent::BasicValue(BasicValue::SearchPath("foo/bar.baz")),
    };
    let value = parse_nix("<foo/bar.baz>".as_bytes()).unwrap();
    assert_eq!(expected, value);
}

#[test]
fn test_attrset_lambda_trailing_comma() {
    let expected = NixExpr {
        position: SourcePosition { line: 1, column: 1 },
        content: crate::ast::NixExprContent::Code(Code::Lambda(Lambda {
            args: crate::ast::LambdaArgs::AttrsetBinding {
                total_name: Some("args"),
                args: LambdaAttrsetArgs {
                    bindings: [("a", None), ("foo", None)].into_iter().collect(),
                    includes_rest_pattern: false,
                },
            },
            body: Box::new(NixExpr {
                position: SourcePosition { line: 1, column: 20 },
                content: crate::ast::NixExprContent::BasicValue(BasicValue::Int(42)),
            }),
        })),
    };
    let value = parse_nix("{a, foo, }@args: 42".as_bytes()).unwrap();
    assert_eq!(expected, value);
}
