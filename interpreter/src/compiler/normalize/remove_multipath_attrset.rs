use parser::ast::{
    AssertExpr, Attrset, AttrsetKey, Code, IfExpr, KnownNixStringContent, NixExpr, NixExprContent,
    NixString, SourcePosition,
};

use crate::compiler::get_null_expr;

use super::Pass;

mod entry_collection;

pub struct RemoveMultipathAndSearchpathPass {
    _inner: (),
}

impl RemoveMultipathAndSearchpathPass {
    pub fn new() -> Self {
        Self { _inner: () }
    }
}

impl Pass<'_> for RemoveMultipathAndSearchpathPass {
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

    fn inspect_attrset(&mut self, attrset: &mut Attrset, _pos: SourcePosition) {
        // first recurse down.
        self.descend_attrset(attrset);

        // and now remove the multipath attrs on this level
        let mut collection = entry_collection::MultipathEntryCollection::new();

        attrset.attrs.retain_mut(|(key, value)| match key {
            parser::ast::AttrsetKey::Single(_) => true,
            parser::ast::AttrsetKey::Multi(multi) => {
                let key = core::mem::take(multi);
                let value = core::mem::replace(value, get_null_expr());
                collection.add_entry(key, value);
                false
            }
        });

        collection.to_attrs(&mut attrset.attrs);
    }

    fn inspect_hasattr<'a>(&mut self, attrset: &mut NixExpr<'a>, path: &mut AttrsetKey<'a>) {
        // first, recurse.
        self.descend_hasattr(attrset, path);

        match path {
            AttrsetKey::Single(_) => {
                // nothing to do here.
                return;
            }
            AttrsetKey::Multi(multipath) => {
                let last_attr = multipath
                    .pop()
                    .expect("multipath shoud have at least 1 entry");
                for attr in multipath.drain(..) {
                    let inner_source = core::mem::replace(attrset, get_null_expr());
                    *attrset = NixExpr {
                        position: inner_source.position,
                        content: parser::ast::NixExprContent::Code(parser::ast::Code::Op(
                            parser::ast::Op::AttrRef {
                                name: attr,
                                default: Some(Box::new(NixExpr {
                                    position: inner_source.position,
                                    content: parser::ast::NixExprContent::CompoundValue(
                                        parser::ast::CompoundValue::Attrset(Attrset {
                                            is_recursive: false,
                                            inherit_keys: Vec::new(),
                                            attrs: Vec::new(),
                                        }),
                                    ),
                                })),
                                left: Box::new(inner_source),
                            },
                        )),
                    };
                }
                *path = AttrsetKey::Single(last_attr);
            }
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
