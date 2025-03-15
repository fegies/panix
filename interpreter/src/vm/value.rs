use std::{ops::Range, path::Path};

use gc::{
    GcError, GcHandle, GcPointer,
    specialized_types::{array::Array, string::SimpleGcString},
};
use gc_derive::Trace;

use crate::{EvaluateError, Evaluator, builtins::BuiltinTypeToken, util::Stackvec};

use super::opcodes::{ExecutionContext, LambdaCallType, VmOp};

pub mod debug;

#[derive(Debug, Trace)]
pub enum Thunk {
    /// A special kind of null value that is not representable in the nix language
    /// and is mainly useful to detect infinite recursions.
    Blackhole,
    Value(NixValue),
    Deferred {
        context: ExecutionContext,
        code: GcPointer<Array<VmOp>>,
    },
}

#[derive(Debug, Trace, Clone)]
pub struct NixString {
    inner: GcPointer<SimpleGcString>,
}
impl NixString {
    pub fn load<'a, 'g>(&'a self, gc_handle: &'g GcHandle) -> &'a str
    where
        'g: 'a,
    {
        gc_handle.load(&self.inner).as_ref()
    }

    pub fn concat(self, other: NixString, gc: &mut GcHandle) -> Result<NixString, GcError> {
        let res = gc.alloc_string_concat(&[self.inner, other.inner])?;
        Ok(NixString { inner: res })
    }

    pub fn concat_many<E>(
        pieces: impl Iterator<Item = Result<NixString, E>>,
        gc: &mut GcHandle,
    ) -> Result<NixString, E>
    where
        E: From<GcError>,
    {
        let mut vec = Stackvec::<64, _>::new();
        for item in pieces {
            if let Some(item) = vec.push(item?.inner) {
                // our vec was full. Execute a concat with the stored pieces
                // to free up some space again.

                let concatted = gc.alloc_string_concat(vec.as_ref())?;
                vec.clear();
                vec.push(concatted);
                // and retry to push the item.
                vec.push(item);
            }
        }

        let result = if vec.len() > 0 {
            gc.alloc_string_concat(vec.as_ref())?
        } else {
            // there were 0 pieces to concat. In this case we emit an empty string.
            gc.alloc_string("")?
        };

        Ok(NixString { inner: result })
    }

    pub fn get_substring(
        &self,
        gc: &mut GcHandle,
        range: Range<usize>,
    ) -> Result<NixString, GcError> {
        let substring = gc.alloc_substring(&self.inner, range)?;
        Ok(NixString { inner: substring })
    }
}
impl From<GcPointer<SimpleGcString>> for NixString {
    fn from(value: GcPointer<SimpleGcString>) -> Self {
        Self { inner: value }
    }
}

impl PartialEq for NixString {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            _ => false,
        }
    }
}

#[derive(Debug, Trace, Clone)]
pub enum NixValue {
    String(NixString),
    Bool(bool),
    Null,
    Int(i64),
    Float(f64),
    Path(PathValue),
    Attrset(Attrset),
    Function(Function),
    List(List),
    Builtin(BuiltinTypeToken),
}

#[derive(Debug, Trace, Clone)]
pub struct RawPathValue {
    pub path_value: NixString,
    pub sourcefile_location: NixString,
}

#[derive(Debug, Trace, Clone)]
pub struct PathValue {
    pub raw: GcPointer<RawPathValue>,
    pub resolved: NixString,
    _private: (),
}

impl PathValue {
    pub fn new(
        sourcefile_location: NixString,
        path_value: NixString,
        gc: &mut GcHandle,
    ) -> Result<Self, EvaluateError> {
        let resolved_path = Path::new(path_value.load(&gc));

        let resolved = if resolved_path.is_relative() {
            let source_dir = Path::new(sourcefile_location.load(&gc))
                .parent()
                .ok_or(EvaluateError::AccessOutOfRange)?;
            let resolved = source_dir.join(resolved_path);
            let resolved = core::str::from_utf8(resolved.as_os_str().as_encoded_bytes())
                .map_err(|e| EvaluateError::Misc(Box::new(e)))?;
            gc.alloc_string(resolved)?.into()
        } else {
            path_value.clone()
        };

        let raw = gc.alloc(RawPathValue {
            path_value,
            sourcefile_location,
        })?;

        Ok(Self {
            raw,
            resolved,
            _private: (),
        })
    }
}

#[derive(Debug, Trace, Clone)]
pub struct Attrset {
    pub entries: GcPointer<Array<(NixString, GcPointer<Thunk>)>>,
}

impl Attrset {
    pub fn get_entry(&self, gc_handle: &GcHandle, key: &NixString) -> Option<GcPointer<Thunk>> {
        let key_str = key.load(gc_handle);
        self.get_entry_str(gc_handle, key_str)
    }
    pub fn get_entry_str(&self, gc_handle: &GcHandle, key_str: &str) -> Option<GcPointer<Thunk>> {
        let attrset_slice = gc_handle.load(&self.entries).as_ref();

        attrset_slice
            .binary_search_by_key(&key_str, |(k, _)| k.load(gc_handle))
            .ok()
            .map(|value_idx| attrset_slice[value_idx].1.clone())
    }

    pub fn get_and_force_entry_str(
        &self,
        evaluator: &mut Evaluator,
        key_str: &str,
    ) -> Result<NixValue, EvaluateError> {
        let entry = self
            .get_entry_str(&evaluator.gc_handle, key_str)
            .ok_or_else(|| EvaluateError::AttrsetKeyNotFound {
                attr_name: key_str.to_owned(),
            })?;
        evaluator.force_thunk(entry)
    }

    pub fn keys<'a>(&'a self, gc_handle: &'a GcHandle) -> impl Iterator<Item = &'a str> {
        let slice = gc_handle.load(&self.entries).as_ref();
        slice.iter().map(|nix_str| nix_str.0.load(gc_handle))
    }

    fn iter_entries<'a>(
        &'a self,
        gc: &'a GcHandle,
    ) -> impl Iterator<Item = (&'a str, &'a GcPointer<Thunk>)> + 'a {
        let entries = gc.load(&self.entries).as_ref();
        entries.into_iter().map(move |(k, v)| (k.load(&gc), v))
    }

    pub fn build_from_entries(
        entries: &mut Vec<(NixString, GcPointer<Thunk>)>,
        gc_handle: &mut GcHandle,
    ) -> Result<Attrset, EvaluateError> {
        let num_keys = entries.len();
        entries.sort_by(|(a, _), (b, _)| a.load(gc_handle).cmp(b.load(gc_handle)));

        entries.dedup_by(|(a, _), (b, _)| a.load(gc_handle) == b.load(gc_handle));

        if entries.len() < num_keys as usize {
            return Err(EvaluateError::DuplicateAttrsetKey);
        }

        let entries = gc_handle.alloc_vec(entries)?;
        Ok(Attrset { entries })
    }

    /// creates a new attribute set by merging other into self
    pub fn merge(self, other: Attrset, gc: &mut GcHandle) -> Result<Attrset, GcError> {
        let mut left_iter = gc
            .load(&self.entries)
            .as_ref()
            .iter()
            .map(|(k, v)| (k.load(&gc), (k.clone(), v.clone())));
        let mut right_iter = gc
            .load(&other.entries)
            .as_ref()
            .iter()
            .map(|(k, v)| (k.load(&gc), (k.clone(), v.clone())));

        let (mut left_key, mut left_entry) = match left_iter.next() {
            Some(f) => f,
            _ => {
                // the left value is completely empty. we can just return the second attrset.
                return Ok(other);
            }
        };
        let (mut right_key, mut right_entry) = match right_iter.next() {
            Some(f) => f,
            _ => {
                // the right value is an empty set. this will not change anything about the result.
                // as such we can just return the input set.
                return Ok(self);
            }
        };

        let mut result_buf = Vec::new();
        loop {
            match left_key.cmp(right_key) {
                std::cmp::Ordering::Less => {
                    // emit the left entry and advance that iterator.
                    result_buf.push(left_entry);
                    match left_iter.next() {
                        Some(next) => {
                            (left_key, left_entry) = next;
                        }
                        _ => {
                            // left iterator exhausted. emit the right one.
                            result_buf.push(right_entry);
                            result_buf.extend(right_iter.map(|(_, e)| e));
                            break;
                        }
                    }
                }
                std::cmp::Ordering::Greater => {
                    // emit the right entry and advance that iterator.
                    result_buf.push(right_entry);
                    match right_iter.next() {
                        Some(next) => {
                            (right_key, right_entry) = next;
                        }
                        _ => {
                            // right iterator exhausted, emit the rest of the left one.
                            result_buf.push(left_entry);
                            result_buf.extend(left_iter.map(|t| t.1));
                            break;
                        }
                    }
                }
                std::cmp::Ordering::Equal => {
                    // an attribute present in both.
                    // the right side takes precedence in being emitted, but we need to advance
                    // both iterators.
                    result_buf.push(right_entry);

                    match right_iter.next() {
                        Some(next) => {
                            (right_key, right_entry) = next;
                        }
                        _ => {
                            result_buf.extend(left_iter.map(|t| t.1));
                            break;
                        }
                    }

                    match left_iter.next() {
                        Some(next) => {
                            (left_key, left_entry) = next;
                        }
                        _ => {
                            result_buf.extend(right_iter.map(|t| t.1));
                            break;
                        }
                    }
                }
            }
        }

        let entries = gc.alloc_vec(&mut result_buf)?;
        Ok(Attrset { entries })
    }
}

impl PartialEq for Attrset {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            _ => false,
        }
    }
}

#[derive(Debug, Trace, Clone)]
pub struct Function {
    /// the captured execution context to be used in the body
    pub context: ExecutionContext,
    pub code: GcPointer<Array<VmOp>>,
    pub call_type: LambdaCallType,
}

#[derive(Debug, Trace, Clone)]
pub struct List {
    pub entries: GcPointer<Array<GcPointer<Thunk>>>,
}

impl List {
    pub fn concat_lists(lists: &[List], gc: &mut GcHandle) -> Result<List, GcError> {
        let mut result_buf: Vec<_> = lists
            .iter()
            .flat_map(|l| gc.load(&l.entries).as_ref())
            .cloned()
            .collect();
        let entries = gc.alloc_vec(&mut result_buf)?;
        Ok(List { entries })
    }

    pub(crate) fn expect_entries<const N: usize>(
        &self,
        gc_handle: &GcHandle,
    ) -> Result<[GcPointer<Thunk>; N], EvaluateError> {
        let slice = gc_handle.load(&self.entries).as_ref();
        let arrayref: &[_; N] = slice
            .try_into()
            .map_err(|_| EvaluateError::AccessOutOfRange)?;
        Ok(arrayref.clone())
    }
}
