use gc::{GcError, GcHandle, GcPointer};
use lookup_scope::ScopeBacking;
use parser::ast::{BasicValue, IfExpr, KnownNixStringContent, NixExpr, NixString, SourcePosition};
use thunk_compiler::ThunkCompiler;

use crate::{
    util::BufferPool,
    vm::{
        opcodes::{ExecutionContext, VmOp},
        value::{self, NixValue, Thunk},
    },
};

mod lookup_scope;
mod normalize;
#[cfg(test)]
mod tests;
mod thunk_compiler;

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("error when allocating")]
    Gc(#[from] GcError),
    #[error("Undefined value: {value} at {pos}")]
    Deref { value: String, pos: SourcePosition },
}

pub fn translate_expression(
    gc_handle: &mut GcHandle,
    mut expr: NixExpr<'_>,
) -> Result<Thunk, CompileError> {
    normalize::normalize_ast(&mut expr);

    let mut key_buffer = Default::default();
    let mut value_buffer = Default::default();
    let mut compiler = Compiler {
        cached_values: CachedValues::new(gc_handle)?,
        gc_handle,
        key_buffer: &mut key_buffer,
        value_buffer: &mut value_buffer,
    };
    let mut scope_backing = ScopeBacking::new();

    ThunkCompiler::new(&mut compiler).translate_to_thunk(scope_backing.build_scope(), expr)
}

struct Compiler<'gc> {
    gc_handle: &'gc mut GcHandle,
    key_buffer: &'gc BufferPool<value::NixString>,
    value_buffer: &'gc BufferPool<Thunk>,
    cached_values: CachedValues,
}

struct CachedValues {
    true_boolean: GcPointer<NixValue>,
    false_boolean: GcPointer<NixValue>,
    empty_string: GcPointer<NixValue>,
}
impl CachedValues {
    fn new(gc_handle: &mut GcHandle) -> Result<Self, GcError> {
        let empty_string = NixValue::String(value::NixString::from(gc_handle.alloc_string("")?));
        Ok(Self {
            true_boolean: gc_handle.alloc(NixValue::Bool(true))?,
            false_boolean: gc_handle.alloc(NixValue::Bool(false))?,
            empty_string: gc_handle.alloc(empty_string)?,
        })
    }
}

impl<'gc> Compiler<'gc> {
    fn alloc_string(
        &mut self,
        str: KnownNixStringContent<'_>,
    ) -> Result<GcPointer<NixValue>, CompileError> {
        let res = match str {
            KnownNixStringContent::Empty | KnownNixStringContent::Literal("") => {
                return Ok(self.cached_values.empty_string.clone())
            }
            KnownNixStringContent::Literal(l) => {
                value::NixString::from(self.gc_handle.alloc_string(l)?)
            }
            KnownNixStringContent::Composite(parts) => {
                value::NixString::from(self.gc_handle.alloc_string_from_parts(&parts)?)
            }
        };
        Ok(self.gc_handle.alloc(NixValue::String(res))?)
    }
}
