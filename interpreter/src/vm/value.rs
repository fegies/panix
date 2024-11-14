use gc::{
    specialized_types::{array::Array, string::SimpleGcString},
    GcPointer,
};
use gc_derive::Trace;

use super::opcodes::{ExecutionContext, VmOp};

#[derive(Debug, Trace)]
pub enum Thunk {
    /// A special kind of null value that is not representable in the nix language
    /// and is mainly useful to detect infinite recursions.
    Blackhole,
    Value(GcPointer<NixValue>),
    Deferred {
        context: ExecutionContext,
        code: GcPointer<Array<VmOp>>,
    },
}

#[derive(Debug, Trace)]
pub enum NixString {
    Empty,
    Simple(GcPointer<SimpleGcString>),
}

impl PartialEq for NixString {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            _ => false,
        }
    }
}

#[derive(Debug, Trace)]
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

#[derive(Debug, Trace)]
pub struct Attrset {
    pub keys: GcPointer<Array<NixString>>,
    pub values: GcPointer<Array<Thunk>>,
}
impl PartialEq for Attrset {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            _ => false,
        }
    }
}

#[derive(Debug, Trace)]
pub struct Function {
    pub args: FunctionArgs,
    pub body: GcPointer<Thunk>,
}

#[derive(Debug)]
pub struct AttrsetFunctionArg {
    pub name: GcPointer<NixString>,
    pub default: Option<Thunk>,
}
#[derive(Debug, Trace)]
pub enum FunctionArgs {
    Single,
    AttrsetArgs {
        entries: GcPointer<Array<AttrsetFunctionArg>>,
        others_allowed: bool,
    },
}

#[derive(Debug, Trace)]
pub struct List {
    pub entries: GcPointer<Array<Thunk>>,
}
