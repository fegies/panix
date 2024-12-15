use gc::{
    specialized_types::{array::Array, string::SimpleGcString},
    GcHandle, GcPointer,
};
use gc_derive::Trace;

use super::opcodes::{ExecutionContext, VmOp};

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
}

#[derive(Debug, Trace, Clone)]
pub struct Attrset {
    pub entries: GcPointer<Array<(NixString, GcPointer<Thunk>)>>,
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
    pub args: FunctionArgs,
    pub body: GcPointer<Thunk>,
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
