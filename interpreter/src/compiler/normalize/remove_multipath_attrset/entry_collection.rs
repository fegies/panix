use std::collections::BTreeMap;

use parser::ast::{CompoundValue, KnownNixStringContent, NixExprContent, NixString};

use super::*;
pub struct MultipathEntryCollection<'a> {
    entries: BTreeMap<KnownNixStringContent<'a>, (SourcePosition, MultipathEntry<'a>)>,
    // attributes that we either found to be duplicate or that need to be evaluated
    // at runtime
    extra_attrs: Vec<(AttrsetKey<'a>, NixExpr<'a>)>,
    is_recursive: bool,
}

enum MultipathEntry<'a> {
    Value(NixExpr<'a>),
    Nested(MultipathEntryCollection<'a>),
}

fn expand_out_attrset_path<'a>(
    key_pieces: impl Iterator<Item = NixString<'a>>,
    mut value: NixExpr<'a>,
) -> NixExpr<'a> {
    for key_piece in key_pieces {
        value = NixExpr {
            position: key_piece.position,
            content: parser::ast::NixExprContent::CompoundValue(
                parser::ast::CompoundValue::Attrset(Attrset {
                    is_recursive: false,
                    inherit_keys: Vec::new(),
                    attrs: vec![(AttrsetKey::Single(key_piece), value)],
                }),
            ),
        }
    }
    value
}

impl<'a> MultipathEntryCollection<'a> {
    pub fn new() -> Self {
        Self {
            entries: BTreeMap::new(),
            extra_attrs: Vec::new(),
            is_recursive: false,
        }
    }

    fn add_entry_inner(
        &mut self,
        first_key: NixString<'a>,
        mut tailing_segments: impl Iterator<Item = NixString<'a>>,
        value: NixExpr<'a>,
    ) {
        match &first_key.content {
            parser::ast::NixStringContent::Known(first_key_content) => {
                let sourcepos = first_key.position;
                // first check if we need to try to pass the value deeper
                // in (there are more tailing segments left)
                match tailing_segments.next() {
                    Some(next_key) => {
                        match self
                            .entries
                            .entry(first_key_content.clone())
                            .or_insert_with(|| {
                                (
                                    sourcepos,
                                    MultipathEntry::Nested(MultipathEntryCollection::new()),
                                )
                            }) {
                            (_, MultipathEntry::Value(_)) => {
                                // we found a conflict!
                                // To defer it to eval time, we only add the kv to the extra attrs
                                self.extra_attrs
                                    .push((AttrsetKey::Single(first_key), value));
                            }
                            (_, MultipathEntry::Nested(next_level)) => {
                                next_level.add_entry_inner(next_key, tailing_segments, value);
                            }
                        }
                    }
                    _ => {
                        // no further key segments.
                        // If the current value is an attrset, we need to break it down further to
                        // enable it to be merged with other items.
                        // Other values are just inserted here.
                        if let NixExprContent::CompoundValue(CompoundValue::Attrset(attrset)) =
                            value.content
                        {
                            let entry = self
                                .entries
                                .entry(first_key_content.clone())
                                .or_insert_with(|| {
                                    (
                                        sourcepos,
                                        MultipathEntry::Nested(MultipathEntryCollection::new()),
                                    )
                                });

                            if let MultipathEntry::Value(v) = &mut entry.1 {
                                let v = core::mem::replace(v, get_null_expr());
                                self.extra_attrs.push((AttrsetKey::Single(first_key), v));
                                entry.1 = MultipathEntry::Nested(MultipathEntryCollection::new());
                            }

                            let next_level = match &mut entry.1 {
                                MultipathEntry::Nested(nested) => nested,
                                _ => unreachable!(), // it must be nested becaue we have just
                                                     // removed the other option above.
                            };

                            if attrset.is_recursive {
                                next_level.is_recursive = true;
                            }

                            for (key, value) in attrset.attrs {
                                next_level.add_entry(key, value);
                            }
                        } else {
                            match self.entries.entry(first_key_content.clone()) {
                                std::collections::btree_map::Entry::Vacant(v) => {
                                    // all ok, we can just put the value in.
                                    v.insert((first_key.position, MultipathEntry::Value(value)));
                                }
                                std::collections::btree_map::Entry::Occupied(_) => {
                                    // we have detected a conflict!
                                    // To defer the error to evaluation time,
                                    // we intentionally write the duped value into the extra_attrs
                                    self.extra_attrs
                                        .push((AttrsetKey::Single(first_key), value));
                                }
                            }
                        }
                    }
                }
            }
            parser::ast::NixStringContent::Interpolated(_) => {
                // since we cannot determine what this key value represents,
                // the only thing we can do is to transform the multipath key
                // into the equivalent stack of attribute set defs
                //
                // Duplicate definition detection must be delayed to runtime
                self.extra_attrs.push((
                    AttrsetKey::Single(first_key),
                    expand_out_attrset_path(tailing_segments, value),
                ))
            }
        }
    }

    pub fn add_entry(&mut self, key: AttrsetKey<'a>, value: NixExpr<'a>) {
        match key {
            AttrsetKey::Single(nix_string) => {
                self.add_entry_inner(nix_string, core::iter::empty(), value)
            }
            AttrsetKey::Multi(vec) => {
                let mut iter = vec.into_iter();
                if let Some(first) = iter.next() {
                    self.add_entry_inner(first, iter, value);
                }
            }
        }
    }

    pub fn to_attrs(mut self, dest_buffer: &mut Vec<(AttrsetKey<'a>, NixExpr<'a>)>) {
        for (key, (sourcepos, entry)) in self.entries {
            let value = match entry {
                MultipathEntry::Value(value) => value,
                MultipathEntry::Nested(nested) => {
                    let mut inner_attrs = Vec::new();
                    let is_recursive = nested.is_recursive;
                    nested.to_attrs(&mut inner_attrs);
                    NixExpr {
                        position: sourcepos,
                        content: parser::ast::NixExprContent::CompoundValue(
                            parser::ast::CompoundValue::Attrset(Attrset {
                                is_recursive,
                                inherit_keys: Vec::new(),
                                attrs: inner_attrs,
                            }),
                        ),
                    }
                }
            };
            let key = AttrsetKey::Single(NixString {
                position: sourcepos,
                content: parser::ast::NixStringContent::Known(key),
            });
            dest_buffer.push((key, value));
        }
        dest_buffer.append(&mut self.extra_attrs);
    }

    pub fn inspect_entries<P: Pass<'a>>(&mut self, pass: &mut P) {
        for (_key, (_, val)) in &mut self.entries {
            match val {
                MultipathEntry::Value(nix_expr) => pass.inspect_expr(nix_expr),
                MultipathEntry::Nested(nested) => nested.inspect_entries(pass),
            }
        }

        for (key, val) in &mut self.extra_attrs {
            match key {
                AttrsetKey::Single(nix_string) => pass.inspect_nix_string(nix_string),
                AttrsetKey::Multi(vec) => {
                    for part in vec {
                        pass.inspect_nix_string(part);
                    }
                }
            }
            pass.inspect_expr(val);
        }
    }
}
