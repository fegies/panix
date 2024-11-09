use parser::ast::{Attrset, AttrsetKey, KnownNixStringContent, NixExpr, NixString, SourcePosition};

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

fn get_null_expr() -> NixExpr<'static> {
    NixExpr {
        position: SourcePosition { line: 0, column: 0 },
        content: parser::ast::NixExprContent::BasicValue(parser::ast::BasicValue::Null),
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
}
