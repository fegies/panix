use gc::GcHandle;

use crate::vm::opcodes::{ThunkAllocArgs, VmOp};

use super::{Attrset, List, NixValue, Thunk};

pub struct ThunkDebug<'a> {
    thunk: &'a Thunk,
    gc: &'a GcHandle,
}

impl Thunk {
    pub fn debug<'a>(&'a self, gc_handle: &'a GcHandle) -> ThunkDebug<'a> {
        ThunkDebug {
            thunk: self,
            gc: gc_handle,
        }
    }
}
impl NixValue {
    pub fn debug<'a>(&'a self, gc_handle: &'a GcHandle) -> ValueDebug<'a> {
        ValueDebug {
            value: self,
            gc: gc_handle,
        }
    }
}

impl core::fmt::Debug for ThunkDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.thunk {
            Thunk::Blackhole => f.debug_tuple("Blackhole").finish(),
            Thunk::Value(v) => f.debug_tuple("Value").field(&v.debug(&self.gc)).finish(),
            Thunk::Deferred {
                context: _,
                code: _,
            } => f.debug_tuple("<<Deferred>>").finish(),
        }
    }
}

pub struct ValueDebug<'a> {
    value: &'a NixValue,
    gc: &'a GcHandle,
}

impl core::fmt::Debug for ValueDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            NixValue::String(s) => f.debug_tuple("String").field(&s.load(&self.gc)).finish(),
            NixValue::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
            NixValue::Null => f.debug_tuple("Null").finish(),
            NixValue::Int(i) => f.debug_tuple("Int").field(i).finish(),
            NixValue::Float(fl) => f.debug_tuple("Float").field(fl).finish(),
            NixValue::Path(p) => f.debug_tuple("Path").field(p).finish(),
            NixValue::Attrset(a) => f
                .debug_tuple("Attrset")
                .field(&AttrsetDebug {
                    value: a,
                    gc: self.gc,
                })
                .finish(),
            NixValue::Function(l) => f.debug_tuple("Function").field(l).finish(),
            NixValue::List(l) => f
                .debug_tuple("List")
                .field(&ListDebug {
                    value: l,
                    gc: &self.gc,
                })
                .finish(),
            NixValue::Builtin(b) => f.debug_tuple("Builtin").field(b).finish(),
        }
    }
}

pub struct ListDebug<'a> {
    value: &'a List,
    gc: &'a GcHandle,
}

impl core::fmt::Debug for ListDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let entries = self
            .gc
            .load(&self.value.entries)
            .as_ref()
            .into_iter()
            .map(|ptr| self.gc.load(ptr).debug(&self.gc));

        f.debug_list().entries(entries).finish()
    }
}

pub struct AttrsetDebug<'a> {
    value: &'a Attrset,
    gc: &'a GcHandle,
}

impl core::fmt::Debug for AttrsetDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut set = f.debug_map();
        for (key, value) in self.value.iter_entries(&self.gc) {
            let value = self.gc.load(value);
            set.entry(&key, &value.debug(&self.gc));
        }
        set.finish()
    }
}

pub struct VmopDebug<'a> {
    op: &'a VmOp,
    gc: &'a GcHandle,
}

impl VmOp {
    pub fn debug<'a>(&'a self, gc: &'a GcHandle) -> VmopDebug<'a> {
        VmopDebug { op: self, gc }
    }
}

impl core::fmt::Debug for VmopDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.op {
            VmOp::PushImmediate(imm) => f
                .debug_tuple("PushImmediate")
                .field(&self.gc.load(imm).debug(&self.gc))
                .finish(),
            VmOp::AllocateThunk { slot, args } => f
                .debug_struct("AllocateThunk")
                .field("slot", slot)
                .field(
                    "args",
                    &ThunkArgsDebug {
                        gc: &self.gc,
                        val: self.gc.load(args),
                    },
                )
                .finish(),
            _ => self.op.fmt(f),
        }
    }
}

struct ThunkArgsDebug<'a> {
    val: &'a ThunkAllocArgs,
    gc: &'a GcHandle,
}

impl core::fmt::Debug for ThunkArgsDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct CodeDebug<'a> {
            code: &'a [VmOp],
            gc: &'a GcHandle,
        }
        impl core::fmt::Debug for CodeDebug<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_list()
                    .entries(self.code.iter().map(|op| op.debug(&self.gc)))
                    .finish()
            }
        }

        f.debug_struct("ThunkAllocArgs")
            .field("context_id", &self.val.context_id)
            .field(
                "context_build_instructions",
                &self.gc.load(&self.val.context_build_instructions).as_ref(),
            )
            .field(
                "code",
                &CodeDebug {
                    gc: &self.gc,
                    code: self.gc.load(&self.val.code).as_ref(),
                },
            )
            .finish()
    }
}
