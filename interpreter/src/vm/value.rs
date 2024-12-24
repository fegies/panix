use gc::{
    specialized_types::{array::Array, string::SimpleGcString},
    GcError, GcHandle, GcPointer,
};
use gc_derive::Trace;

use crate::{builtins::BuiltinTypeToken, util::Stackvec};

use super::opcodes::{ExecutionContext, LambdaCallType, VmOp};

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
    Path(NixString),
    Attrset(Attrset),
    Function(Function),
    List(List),
    Builtin(BuiltinTypeToken),
}

#[derive(Debug, Trace, Clone)]
pub struct Attrset {
    pub entries: GcPointer<Array<(NixString, GcPointer<Thunk>)>>,
}

impl Attrset {
    pub fn get_entry(&self, gc_handle: &GcHandle, key: &NixString) -> Option<GcPointer<Thunk>> {
        let attrset_slice = gc_handle.load(&self.entries).as_ref();
        let key_str = key.load(gc_handle);

        attrset_slice
            .binary_search_by_key(&key_str, |(k, _)| k.load(gc_handle))
            .ok()
            .map(|value_idx| attrset_slice[value_idx].1.clone())
    }

    pub fn keys<'a>(&'a self, gc_handle: &'a GcHandle) -> impl Iterator<Item = &'a str> {
        let slice = gc_handle.load(&self.entries).as_ref();
        slice.iter().map(|nix_str| nix_str.0.load(gc_handle))
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

#[derive(Debug)]
pub struct AttrsetFunctionArg {
    pub name: GcPointer<NixString>,
    pub default: Option<Thunk>,
}
#[derive(Debug, Trace, Clone)]
pub enum FunctionArgs {
    Single,
    AttrsetArgs {
        entries: GcPointer<Array<AttrsetFunctionArg>>,
        others_allowed: bool,
    },
}

#[derive(Debug, Trace, Clone)]
pub struct List {
    pub entries: GcPointer<Array<GcPointer<Thunk>>>,
}
