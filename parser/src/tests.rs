use lexer::SourcePosition;

use crate::{
    ast::{BasicValue, BinopOpcode, NixExpr},
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
