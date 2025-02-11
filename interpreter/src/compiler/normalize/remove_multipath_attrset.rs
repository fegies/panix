use parser::ast::{Attrset, AttrsetKey, Code, List, NixExpr, Op, SourcePosition};

use crate::compiler::get_null_expr;

use super::Pass;

mod entry_collection;

pub struct RemoveMultipathPass {
    _inner: (),
}

impl RemoveMultipathPass {
    pub fn new() -> Self {
        Self { _inner: () }
    }
}

impl Pass<'_> for RemoveMultipathPass {
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

    fn inspect_expr(&mut self, expr: &mut NixExpr<'_>) {
        self.descend_expr(expr);

        if let parser::ast::NixExprContent::Code(Code::Op(Op::HasAttr {
            left,
            path: AttrsetKey::Multi(multipath),
        })) = &mut expr.content
        {
            let inner = core::mem::replace(left.as_mut(), get_null_expr());
            let position = inner.position;
            let list = NixExpr {
                position,
                content: parser::ast::NixExprContent::CompoundValue(
                    parser::ast::CompoundValue::List(List {
                        entries: multipath
                            .drain(..)
                            .map(|str| NixExpr {
                                position: inner.position,
                                content: parser::ast::NixExprContent::BasicValue(
                                    parser::ast::BasicValue::String(str),
                                ),
                            })
                            .collect(),
                    }),
                ),
            };

            *expr = NixExpr {
                position,
                content: parser::ast::NixExprContent::Code(Code::Op(Op::Call {
                    function: Box::new(NixExpr {
                        position,
                        content: parser::ast::NixExprContent::Code(Code::ValueReference {
                            ident: "___builtin_hasattr_multi",
                        }),
                    }),
                    arg: Box::new(NixExpr {
                        position,
                        content: parser::ast::NixExprContent::CompoundValue(
                            parser::ast::CompoundValue::List(List {
                                entries: vec![inner, list],
                            }),
                        ),
                    }),
                })),
            };
        }
    }
}
