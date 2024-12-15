use parser::ast::{Attrset, AttrsetKey, KnownNixStringContent, NixExpr, NixString, SourcePosition};

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

impl Pass for RemoveMultipathPass {
    fn inspect_attrset(&mut self, attrset: &mut Attrset) {
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
