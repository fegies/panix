use std::collections::{BTreeMap, HashSet};

use super::*;
pub struct MultipathEntryCollection<'a> {
    entries: BTreeMap<KnownNixStringContent<'a>, (SourcePosition, MultipathEntry<'a>)>,
    // attributes that we either found to be duplicate or that need to be evaluated
    // at runtime
    extra_attrs: Vec<(AttrsetKey<'a>, NixExpr<'a>)>,
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
                if let Some(next_key) = tailing_segments.next() {
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
                } else {
                    // no further key segments. We can just try to insert here.
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

    pub fn add_entry(&mut self, key_segments: Vec<NixString<'a>>, value: NixExpr<'a>) {
        let mut iter = key_segments.into_iter();
        if let Some(first) = iter.next() {
            self.add_entry_inner(first, iter, value);
        }
    }

    pub fn to_attrs(mut self, dest_buffer: &mut Vec<(AttrsetKey<'a>, NixExpr<'a>)>) {
        for (key, (sourcepos, entry)) in self.entries {
            let value = match entry {
                MultipathEntry::Value(value) => value,
                MultipathEntry::Nested(nested) => {
                    let mut inner_attrs = Vec::new();
                    nested.to_attrs(&mut inner_attrs);
                    NixExpr {
                        position: sourcepos,
                        content: parser::ast::NixExprContent::CompoundValue(
                            parser::ast::CompoundValue::Attrset(Attrset {
                                is_recursive: false,
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
}
