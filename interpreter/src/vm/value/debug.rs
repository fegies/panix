use gc::GcHandle;

use crate::vm::{
    opcodes::{LambdaCallType, ThunkAllocArgs, VmOp},
    value,
};

use super::{Attrset, Function, List, NixValue, Thunk};

pub struct ThunkDebug<'a> {
    thunk: &'a Thunk,
    gc: &'a GcHandle,
    depth: u32,
}

impl Thunk {
    pub fn debug<'a>(&'a self, gc_handle: &'a GcHandle) -> ThunkDebug<'a> {
        self.debug_depth(gc_handle, 0)
    }

    fn debug_depth<'a>(&'a self, gc_handle: &'a GcHandle, depth: u32) -> ThunkDebug<'a> {
        ThunkDebug {
            thunk: self,
            gc: gc_handle,
            depth: depth + 1,
        }
    }
}
impl NixValue {
    pub fn debug<'a>(&'a self, gc_handle: &'a GcHandle) -> ValueDebug<'a> {
        self.debug_depth(gc_handle, 0)
    }

    fn debug_depth<'a>(&'a self, gc_handle: &'a GcHandle, depth: u32) -> ValueDebug<'a> {
        ValueDebug {
            value: self,
            gc: gc_handle,
            depth: depth + 1,
        }
    }
}

impl core::fmt::Debug for ThunkDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.depth > 1 {
            f.write_str("<<elided>>")
        } else {
            match self.thunk {
                Thunk::Blackhole => f.debug_tuple("Blackhole").finish(),
                Thunk::Value(v) => f
                    .debug_tuple("Value")
                    .field(&v.debug_depth(&self.gc, self.depth))
                    .finish(),
                Thunk::Deferred {
                    context: _,
                    code: _,
                } => f.debug_tuple("<<Deferred>>").finish(),
            }
        }
    }
}

pub struct ValueDebug<'a> {
    value: &'a NixValue,
    gc: &'a GcHandle,
    depth: u32,
}

struct FunctionDebug<'a> {
    value: &'a Function,
    gc: &'a GcHandle,
    depth: u32,
}

impl core::fmt::Debug for FunctionDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct ArgDebug<'a> {
            value: &'a LambdaCallType,
            gc: &'a GcHandle,
        }
        struct KeyDebug<'a> {
            key: &'a value::NixString,
            gc: &'a GcHandle,
            is_required: bool,
        }
        impl core::fmt::Debug for KeyDebug<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("Arg")
                    .field("key", &self.key.load(&self.gc))
                    .field("is_required", &self.is_required)
                    .finish()
            }
        }
        struct ArgsListDebug<'a> {
            args: &'a [(value::NixString, bool)],
            gc: &'a GcHandle,
        }
        impl core::fmt::Debug for ArgsListDebug<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_list()
                    .entries(self.args.iter().map(|(key, is_required)| KeyDebug {
                        gc: self.gc,
                        key,
                        is_required: *is_required,
                    }))
                    .finish()
            }
        }

        impl core::fmt::Debug for ArgDebug<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.value {
                    LambdaCallType::Simple => f.debug_tuple("Simple").finish(),
                    LambdaCallType::Attrset {
                        keys,
                        includes_rest_pattern,
                    } => {
                        let keys = self.gc.load(keys).as_ref();
                        f.debug_struct("AttrsetArgs")
                            .field("allow_default", includes_rest_pattern)
                            .field(
                                "keys",
                                &ArgsListDebug {
                                    args: keys,
                                    gc: &self.gc,
                                },
                            )
                            .finish()
                    }
                }
            }
        }

        f.debug_struct("Function")
            .field(
                "args",
                &ArgDebug {
                    value: &self.value.call_type,
                    gc: self.gc,
                },
            )
            .field("context", &"<<elided>>")
            .field(
                "code",
                &CodeDebug {
                    code: self.gc.load(&self.value.code).as_ref(),
                    gc: self.gc,
                    depth: self.depth + 1,
                },
            )
            .finish()
    }
}

pub struct StringDebug<'a> {
    value: &'a value::NixString,
    gc: &'a GcHandle,
}

impl core::fmt::Debug for StringDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", &self.value.load(self.gc)))
    }
}

impl value::NixString {
    pub fn debug<'a>(&'a self, gc: &'a GcHandle) -> StringDebug<'a> {
        StringDebug { value: self, gc }
    }
}

impl core::fmt::Debug for ValueDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            NixValue::String(s) => f.debug_tuple("String").field(&s.load(&self.gc)).finish(),
            NixValue::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
            NixValue::Null => f.debug_tuple("Null").finish(),
            NixValue::Int(i) => f.debug_tuple("Int").field(i).finish(),
            NixValue::Float(fl) => f.debug_tuple("Float").field(fl).finish(),
            NixValue::Path(p) => {
                let raw = self.gc.load(&p.raw);
                f.debug_struct("Path")
                    .field("resolved", &p.resolved.load(&self.gc))
                    .field("source_location", &raw.sourcefile_location.load(&self.gc))
                    .field("value", &raw.path_value.load(&self.gc))
                    .finish()
            }
            NixValue::Attrset(a) => f
                .debug_tuple("Attrset")
                .field(&AttrsetDebug {
                    value: a,
                    gc: self.gc,
                    depth: self.depth,
                })
                .finish(),
            NixValue::Function(l) => f
                .debug_tuple("Function")
                .field(&FunctionDebug {
                    value: l,
                    gc: self.gc,
                    depth: self.depth + 1,
                })
                .finish(),
            NixValue::List(l) => f
                .debug_tuple("List")
                .field(&ListDebug {
                    value: l,
                    gc: &self.gc,
                    depth: self.depth,
                })
                .finish(),
            NixValue::Builtin(b) => f.debug_tuple("Builtin").field(b).finish(),
        }
    }
}

pub struct ListDebug<'a> {
    value: &'a List,
    gc: &'a GcHandle,
    depth: u32,
}

impl core::fmt::Debug for ListDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let entries = self
            .gc
            .load(&self.value.entries)
            .as_ref()
            .into_iter()
            .map(|ptr| self.gc.load(ptr).debug_depth(&self.gc, self.depth));

        f.debug_list().entries(entries).finish()
    }
}

pub struct AttrsetDebug<'a> {
    value: &'a Attrset,
    gc: &'a GcHandle,
    depth: u32,
}

impl core::fmt::Debug for AttrsetDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut set = f.debug_map();
        for (key, value) in self.value.iter_entries(&self.gc) {
            let value = self.gc.load(value);
            set.entry(&key, &value.debug_depth(&self.gc, self.depth));
        }
        set.finish()
    }
}

pub struct VmopDebug<'a> {
    op: &'a VmOp,
    gc: &'a GcHandle,
    depth: u32,
}

impl VmOp {
    pub fn debug<'a>(&'a self, gc: &'a GcHandle) -> VmopDebug<'a> {
        self.debug_depth(gc, 0)
    }
    fn debug_depth<'a>(&'a self, gc: &'a GcHandle, depth: u32) -> VmopDebug<'a> {
        VmopDebug {
            op: self,
            gc,
            depth,
        }
    }
}

impl core::fmt::Debug for VmopDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.op {
            VmOp::PushImmediate(imm) => f
                .debug_tuple("PushImmediate")
                .field(&self.gc.load(imm).debug_depth(&self.gc, self.depth))
                .finish(),
            VmOp::AllocateThunk(args) => f
                .debug_tuple("AllocateThunk")
                .field(&ThunkArgsDebug {
                    gc: &self.gc,
                    val: self.gc.load(args),
                    depth: self.depth,
                })
                .finish(),
            VmOp::CastToPath { source_location } => f
                .debug_struct("CastToPath")
                .field("source_location", &source_location.load(&self.gc))
                .finish(),
            _ => self.op.fmt(f),
        }
    }
}

struct ThunkArgsDebug<'a> {
    val: &'a ThunkAllocArgs,
    gc: &'a GcHandle,
    depth: u32,
}

impl core::fmt::Debug for ThunkArgsDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.depth > 1 {
            return f.write_str("<<elided>>");
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
                    depth: self.depth + 1,
                },
            )
            .finish()
    }
}

struct CodeDebug<'a> {
    code: &'a [VmOp],
    gc: &'a GcHandle,
    depth: u32,
}
impl core::fmt::Debug for CodeDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(
                self.code
                    .iter()
                    .map(|op| op.debug_depth(&self.gc, self.depth)),
            )
            .finish()
    }
}
