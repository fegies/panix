use std::collections::{btree_map::Entry, BTreeMap};

use bumpalo::Bump;
use parser::ast::{
    Attrset, AttrsetKey, Code, Lambda, LetInExpr, NixExpr, NixExprContent, NixString,
    SourcePosition, WithExpr,
};

use crate::compiler::get_null_expr;

use super::Pass;

pub struct RemoveWithExprPass<'src> {
    known_idents: BTreeMap<&'src str, u32>,
    free_idents: Vec<&'src str>,
    used_with_ident_stack: Vec<&'src str>,
    ident_sequence_counter: usize,
    arena: &'src Bump,
}

impl<'a> RemoveWithExprPass<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        let mut known_idents = BTreeMap::new();
        // we need to preload some idents that are predefined by the language.
        known_idents.insert("true", 1);
        known_idents.insert("false", 1);
        known_idents.insert("null", 1);
        known_idents.insert("builtins", 1);

        Self {
            known_idents,
            free_idents: Vec::new(),
            used_with_ident_stack: Vec::new(),
            arena,
            ident_sequence_counter: 0,
        }
    }

    fn get_with_ident(&mut self) -> &'a str {
        self.free_idents.pop().unwrap_or_else(|| {
            let ident_num = self.ident_sequence_counter;
            self.ident_sequence_counter += 1;
            let ident = format!("<!with_{ident_num}>");
            self.arena.alloc_str(&ident)
        })
    }

    fn add_known_key(&mut self, key: &'a str) {
        let refcount = self.known_idents.entry(key).or_default();
        *refcount += 1;
    }

    fn remove_known_key(&mut self, key: &'a str) {
        if let Entry::Occupied(mut e) = self.known_idents.entry(key) {
            let refcount = e.get_mut();
            *refcount -= 1;
            if *refcount == 0 {
                e.remove_entry();
            }
        }
    }

    fn build_lookup_instruction(
        &self,
        ident: &'a str,
        pos: SourcePosition,
    ) -> Option<NixExprContent<'a>> {
        fn construct_getattr<'a>(
            ident: &'a str,
            with_ident: &'a str,
            default: Option<Box<NixExpr<'a>>>,
            pos: SourcePosition,
        ) -> NixExprContent<'a> {
            let left = Box::new(NixExpr {
                position: pos,
                content: NixExprContent::Code(Code::ValueReference { ident: &with_ident }),
            });

            let attrref = parser::ast::Op::AttrRef {
                left,
                path: AttrsetKey::Single(NixString::from_literal(ident, pos)),
                default,
            };
            NixExprContent::Code(Code::Op(attrref))
        }

        let mut iter = self.used_with_ident_stack.iter();

        let mut result = construct_getattr(ident, iter.next()?, None, pos);

        while let Some(with_ident) = iter.next() {
            let default = Box::new(NixExpr {
                position: pos,
                content: result,
            });
            result = construct_getattr(ident, with_ident, Some(default), pos);
        }

        Some(result)
    }
}

impl<'src> Pass<'src> for RemoveWithExprPass<'src> {
    fn inspect_expr(&mut self, expr: &mut NixExpr<'src>) {
        match &mut expr.content {
            NixExprContent::Code(Code::WithExpr(WithExpr { binding, body })) => {
                // first, descend the binding
                self.inspect_expr(binding.as_mut());

                let ident = self.get_with_ident();
                self.used_with_ident_stack.push(ident);

                // now, we can just descend into the body as normal to replace any unknown refs
                self.inspect_expr(body.as_mut());

                let mut bindings = BTreeMap::new();
                bindings.insert(ident, core::mem::replace(binding.as_mut(), get_null_expr()));
                let let_expr = LetInExpr {
                    bindings,
                    inherit_entries: Vec::new(),
                    body: core::mem::replace(body, Box::new(get_null_expr())),
                };

                expr.content =
                    NixExprContent::Code(Code::LetExpr(parser::ast::LetExpr::LetIn(let_expr)));

                // and, at this point the ident we used is no longer active and can be removed
                if let Some(ident) = self.used_with_ident_stack.pop() {
                    self.free_idents.push(ident);
                }
            }

            NixExprContent::Code(Code::ValueReference { ident }) => {
                if !self.known_idents.contains_key(ident) {
                    // whatever ident we were referencing does not actually exist.
                    // instead we replace it by a getattr chain from the known with expressions.
                    // if it fails because no with expr is defined, we just let it fail at compile
                    // time.
                    if let Some(replacement) = self.build_lookup_instruction(ident, expr.position) {
                        expr.content = replacement;
                    }
                }
            }

            _ => {
                // nothing special is happening here. We can just descend the tree as normal.
                self.descend_expr(expr);
            }
        }
    }

    fn inspect_let_in_expr(&mut self, letexpr: &mut LetInExpr<'src>) {
        // first announce the presence of the added keys
        for key in letexpr.bindings.keys().chain(
            letexpr
                .inherit_entries
                .iter()
                .flat_map(|ih| ih.entries.iter()),
        ) {
            self.add_known_key(key)
        }

        // descend down
        self.descend_let_expr(letexpr);

        // and remove the keys again.
        for key in letexpr.bindings.keys().chain(
            letexpr
                .inherit_entries
                .iter()
                .flat_map(|ih| ih.entries.iter()),
        ) {
            self.remove_known_key(key)
        }
    }

    fn inspect_lambda(&mut self, lambda: &mut Lambda<'src>) {
        match &mut lambda.args {
            parser::ast::LambdaArgs::SimpleBinding(name) => {
                self.add_known_key(name);
                self.inspect_expr(lambda.body.as_mut());
                self.remove_known_key(name);
            }
            parser::ast::LambdaArgs::AttrsetBinding { total_name, args } => {
                // make the keys visible
                if let Some(total_name) = total_name {
                    self.add_known_key(&total_name);
                }
                for key in args.bindings.keys() {
                    self.add_known_key(key);
                }

                // descend the tree
                self.inspect_expr(lambda.body.as_mut());
                for subexpr in args.bindings.values_mut().filter_map(|a| a.as_mut()) {
                    self.inspect_expr(subexpr);
                }

                // and unpublish the keys
                if let Some(total_name) = total_name {
                    self.remove_known_key(&total_name);
                }
                for key in args.bindings.keys() {
                    self.remove_known_key(key);
                }
            }
        }
    }

    fn inspect_attrset(&mut self, attrset: &mut Attrset<'src>, pos: SourcePosition) {
        // first, descend down.
        self.descend_attrset(attrset);

        // and now replace all unknown inherit entries.
        let mut keys_to_load_from_with = Vec::new();

        attrset.inherit_keys.retain_mut(|ik| {
            if ik.source.is_none() {
                // we are inhering from the context.
                // remove the keys that are not properly defined.
                ik.entries.retain(|key| {
                    if self.known_idents.contains_key(key) {
                        true
                    } else {
                        if let Some(lookup_instruction) = self.build_lookup_instruction(key, pos) {
                            keys_to_load_from_with.push((*key, lookup_instruction));
                            false
                        } else {
                            true
                        }
                    }
                })
            }
            !ik.entries.is_empty()
        });

        // at this point we have removed the not loadable keys from inherit.
        // to ensure they are loaded properly, we just add them to the attrset as normal entries
        // with a body that loads from the inherit.
        for (ident, content) in keys_to_load_from_with {
            let key = AttrsetKey::Single(NixString::from_literal(ident, pos));
            let value = NixExpr {
                position: pos,
                content,
            };
            attrset.attrs.push((key, value));
        }
    }
}
