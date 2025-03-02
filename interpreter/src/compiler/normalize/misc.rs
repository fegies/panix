use parser::ast::{
    AssertExpr, Attrset, Code, IfExpr, KnownNixStringContent, LetExpr, LetInExpr, NixExpr,
    NixExprContent, NixString, SourcePosition, WithExpr,
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

    fn inspect_code(&mut self, code: &mut Code<'_>, pos: SourcePosition) {
        // first, descend.
        self.descend_code(code, pos);

        if let Code::AssertExpr(AssertExpr { assertion, value }) = code {
            let if_expr = IfExpr {
                falsy_case: Box::new(build_throw_expression(assertion.position)),
                condition: core::mem::replace(assertion, Box::new(get_null_expr())),
                truthy_case: core::mem::replace(value, Box::new(get_null_expr())),
            };
            *code = Code::IfExpr(if_expr);
        }
    }

    fn inspect_let_expr(&mut self, expr: &mut LetExpr, position: SourcePosition) {
        match expr {
            LetExpr::LetIn(let_in_expr) => {
                self.inspect_let_in_expr(let_in_expr);
            }
            LetExpr::AttrsetLet(attrset) => {
                // we do want to remove this obsolete form...
                // to achieve that we replace it with a with and getattr.

                let attrset = core::mem::replace(
                    attrset,
                    Attrset {
                        is_recursive: false,
                        inherit_keys: Vec::new(),
                        attrs: Vec::new(),
                    },
                );
                let attrset = NixExpr {
                    position,
                    content: NixExprContent::CompoundValue(parser::ast::CompoundValue::Attrset(
                        attrset,
                    )),
                };

                let name = "!attrset_let";
                let nameref = NixExpr {
                    position,
                    content: NixExprContent::Code(Code::ValueReference { ident: name }),
                };

                // attrset_let.body
                let body = NixExpr {
                    position,
                    content: NixExprContent::Code(Code::Op(parser::ast::Op::AttrRef {
                        left: Box::new(nameref.clone()),
                        path: parser::ast::AttrsetKey::Single(NixString::from_literal(
                            "body", position,
                        )),
                        default: None,
                    })),
                };

                // with !attrset_let; <<source>>
                let with_expr = NixExpr {
                    position,
                    content: NixExprContent::Code(Code::WithExpr(WithExpr {
                        binding: Box::new(nameref),
                        body: Box::new(attrset),
                    })),
                };

                // let attrset_let = with attrset_let; <<source>>; in attrset_let.body
                let mut outer_let = LetInExpr {
                    bindings: [(name, with_expr)].into_iter().collect(),
                    inherit_entries: Vec::new(),
                    body: Box::new(body),
                };

                // continue our pass downward
                self.inspect_let_in_expr(&mut outer_let);
                *expr = LetExpr::LetIn(outer_let);
            }
        };
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
            path: parser::ast::AttrsetKey::Single(NixString {
                position,
                content: parser::ast::NixStringContent::Known(KnownNixStringContent::Literal(
                    "findFile",
                )),
            }),
            default: None,
        })),
    };
    let nix_path_expr = NixExpr {
        position,
        content: NixExprContent::Code(parser::ast::Code::Op(parser::ast::Op::AttrRef {
            left: Box::new(builtins_expr),
            path: parser::ast::AttrsetKey::Single(NixString {
                position,
                content: parser::ast::NixStringContent::Known(KnownNixStringContent::Literal(
                    "nixPath",
                )),
            }),
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
