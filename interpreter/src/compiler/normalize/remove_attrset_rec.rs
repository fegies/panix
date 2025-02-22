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
            let mut attrset_attrs = Vec::new();
            let mut attrset_inherit_keys = Vec::new();

            let mut parent_let_inherit_entries = Vec::new();
            for attrset_inherit_entry in core::mem::take(inherit_keys) {
                // ensure that the attrset retains all inherit entries
                attrset_inherit_keys.extend(&attrset_inherit_entry.entries);

                // entries with source will need to go to the let too.
                if attrset_inherit_entry.source.is_some() {
                    parent_let_inherit_entries.push(attrset_inherit_entry);
                }
            }

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

            let mut body = NixExpr {
                position: expr.position,
                content: NixExprContent::CompoundValue(parser::ast::CompoundValue::Attrset(
                    Attrset {
                        is_recursive: false,
                        inherit_keys,
                        attrs: attrset_attrs,
                    },
                )),
            };

            if bindings.contains_key("__overrides") {
                // this is a weird undocumented special case for recursive attrsets.
                // effectively, it transforms an attrset a containing the __overrides key
                // to a // a.__overrides
                //
                // Since we will already have lifted the overrides binding, we can just
                // reference it directly.
                let position = body.position;

                let overrides_ref = NixExpr {
                    position,
                    content: NixExprContent::Code(parser::ast::Code::ValueReference {
                        ident: "__overrides",
                    }),
                };

                body = NixExpr {
                    position,
                    content: NixExprContent::Code(parser::ast::Code::Op(parser::ast::Op::Binop {
                        opcode: parser::ast::BinopOpcode::AttrsetMerge,
                        left: Box::new(body),
                        right: Box::new(overrides_ref),
                    })),
                };
            }

            let let_expr = LetInExpr {
                bindings,
                inherit_entries: parent_let_inherit_entries,
                body: Box::new(body),
            };
            expr.content = NixExprContent::Code(parser::ast::Code::LetExpr(
                parser::ast::LetExpr::LetIn(let_expr),
            ));
        }
    }
}
