use parser::ast::{
    AssertExpr, Code, IfExpr, KnownNixStringContent, NixExpr, NixExprContent, NixString,
    SourcePosition,
};

use crate::compiler::get_null_expr;

use super::Pass;

pub struct MiscPass {}

impl Pass<'_> for MiscPass {
    fn inspect_expr(&mut self, expr: &mut NixExpr<'_>) {
        if let NixExprContent::BasicValue(parser::ast::BasicValue::SearchPath(search_path)) =
            expr.content
        {
            expr.content = build_search_path_replacement(search_path, expr.position);
        } else {
            self.descend_expr(expr)
        }
    }

    fn inspect_code(&mut self, code: &mut Code<'_>) {
        // first, descend.
        self.descend_code(code);

        if let Code::AssertExpr(AssertExpr { assertion, value }) = code {
            let if_expr = IfExpr {
                falsy_case: Box::new(build_throw_expression(assertion.position)),
                condition: core::mem::replace(assertion, Box::new(get_null_expr())),
                truthy_case: core::mem::replace(value, Box::new(get_null_expr())),
            };
            *code = Code::IfExpr(if_expr);
        }
    }
}

fn build_throw_expression(position: SourcePosition) -> NixExpr<'static> {
    let lookup_expr = NixExpr {
        position,
        content: NixExprContent::Code(Code::ValueReference { ident: "throw" }),
    };
    let arg_expr = NixExpr {
        position,
        content: NixExprContent::BasicValue(parser::ast::BasicValue::String(NixString {
            position,
            content: parser::ast::NixStringContent::Known(KnownNixStringContent::Literal(
                "assert failed",
            )),
        })),
    };
    NixExpr {
        position,
        content: NixExprContent::Code(Code::Op(parser::ast::Op::Call {
            function: Box::new(lookup_expr),
            arg: Box::new(arg_expr),
        })),
    }
}

/// builds an expression of the form
/// builtins.findFile builtins.nixPath "searchPath"
fn build_search_path_replacement(
    search_path: &str,
    position: SourcePosition,
) -> NixExprContent<'_> {
    let builtins_expr = NixExpr {
        position,
        content: NixExprContent::Code(parser::ast::Code::ValueReference { ident: "builtins" }),
    };
    let find_file_expr = NixExpr {
        position,
        content: NixExprContent::Code(parser::ast::Code::Op(parser::ast::Op::AttrRef {
            left: Box::new(builtins_expr.clone()),
            name: NixString {
                position,
                content: parser::ast::NixStringContent::Known(KnownNixStringContent::Literal(
                    "findFile",
                )),
            },
            default: None,
        })),
    };
    let nix_path_expr = NixExpr {
        position,
        content: NixExprContent::Code(parser::ast::Code::Op(parser::ast::Op::AttrRef {
            left: Box::new(builtins_expr),
            name: NixString {
                position,
                content: parser::ast::NixStringContent::Known(KnownNixStringContent::Literal(
                    "nixPath",
                )),
            },
            default: None,
        })),
    };

    let replacement_content = NixExprContent::Code(parser::ast::Code::Op(parser::ast::Op::Call {
        function: Box::new(NixExpr {
            position,
            content: NixExprContent::Code(parser::ast::Code::Op(parser::ast::Op::Call {
                function: Box::new(find_file_expr),
                arg: Box::new(nix_path_expr),
            })),
        }),
        arg: Box::new(NixExpr {
            position,
            content: NixExprContent::BasicValue(parser::ast::BasicValue::String(NixString {
                position,
                content: parser::ast::NixStringContent::Known(KnownNixStringContent::Literal(
                    search_path,
                )),
            })),
        }),
    }));

    replacement_content
}
