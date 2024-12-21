use std::collections::BTreeMap;

use parser::ast::{Attrset, InheritEntry, LetInExpr, NixExpr, NixExprContent, NixString};

use super::Pass;

pub struct RemoveAttrsetRecPass {
    _inner: (),
}

impl RemoveAttrsetRecPass {
    pub fn new() -> Self {
        Self { _inner: () }
    }
}

impl Pass<'_> for RemoveAttrsetRecPass {
    fn inspect_expr(&mut self, expr: &mut parser::ast::NixExpr) {
        // first, descend
        self.descend_expr(expr);

        if let NixExprContent::CompoundValue(parser::ast::CompoundValue::Attrset(Attrset {
            is_recursive: true,
            inherit_keys,
            attrs,
        })) = &mut expr.content
        {
            let parent_let_inherit_entries = core::mem::take(inherit_keys);

            let mut attrset_attrs = Vec::new();
            let mut attrset_inherit_keys = Vec::new();

            let mut bindings = BTreeMap::new();

            for (key, body) in core::mem::take(attrs) {
                match &key {
                    parser::ast::AttrsetKey::Single(NixString {
                        content:
                            parser::ast::NixStringContent::Known(
                                parser::ast::KnownNixStringContent::Literal(lit),
                            ),
                        position: _,
                    }) => {
                        bindings.insert(*lit, body);
                        attrset_inherit_keys.push(*lit);
                    }
                    _ => {
                        // we cannot interpret it at compile time, just pass it through
                        attrset_attrs.push((key, body));
                    }
                }
            }

            let inherit_keys = if attrset_inherit_keys.is_empty() {
                Vec::new()
            } else {
                vec![InheritEntry {
                    source: None,
                    entries: attrset_inherit_keys,
                }]
            };

            let body = NixExpr {
                position: expr.position,
                content: NixExprContent::CompoundValue(parser::ast::CompoundValue::Attrset(
                    Attrset {
                        is_recursive: false,
                        inherit_keys,
                        attrs: attrset_attrs,
                    },
                )),
            };

            let let_expr = NixExprContent::Code(parser::ast::Code::LetInExpr(LetInExpr {
                bindings,
                inherit_entries: parent_let_inherit_entries,
                body: Box::new(body),
            }));

            expr.content = let_expr;
        }
    }
}
