use std::collections::HashSet;

use lexer::SourcePosition;

use crate::{
    ast::{Attrset, BasicValue, BinopOpcode, Code, CompoundValue, NixExpr, NixString, Op},
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
                    Attrset {
                        is_recursive: false,
                        inherit_keys: HashSet::new(),
                        attrs: Vec::new(),
                    },
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
