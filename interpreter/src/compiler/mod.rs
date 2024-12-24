use bumpalo::Bump;
use gc::{GcError, GcHandle, GcPointer};
use lookup_scope::ScopeBacking;
use parser::ast::{BasicValue, IfExpr, KnownNixStringContent, NixExpr, NixString, SourcePosition};
use thunk_compiler::ThunkCompiler;

use crate::vm::{
    opcodes::{ExecutionContext, VmOp},
    value::{self, NixValue, Thunk},
};

mod lookup_scope;
mod normalize;
mod thunk_compiler;

fn get_null_expr() -> NixExpr<'static> {
    NixExpr {
        position: SourcePosition { line: 0, column: 0 },
        content: parser::ast::NixExprContent::BasicValue(parser::ast::BasicValue::Null),
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ValueSource {
    ContextReference(u32),
    ThunkStackRef(u32),
}
#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("error when allocating")]
    Gc(#[from] GcError),
    #[error("Undefined value: {value} at {pos}")]
    Deref { value: String, pos: SourcePosition },
}

pub fn translate_expression<'src>(
    gc_handle: &mut GcHandle,
    mut expr: NixExpr<'src>,
    bump: &'src Bump,
) -> Result<Thunk, CompileError> {
    normalize::normalize_ast(&mut expr, bump);

    let mut compiler = Compiler {
        cached_values: CachedValues::new(gc_handle)?,
        gc_handle,
    };
    let mut scope_backing = ScopeBacking::new();

    ThunkCompiler::new(&mut compiler).translate_to_thunk(scope_backing.build_scope(), expr)
}

struct Compiler<'gc> {
    gc_handle: &'gc mut GcHandle,
    cached_values: CachedValues,
}

struct CachedValues {
    true_boolean: GcPointer<NixValue>,
    false_boolean: GcPointer<NixValue>,
    null_value: GcPointer<NixValue>,
    empty_string: GcPointer<NixValue>,
}

impl CachedValues {
    fn new(gc_handle: &mut GcHandle) -> Result<Self, GcError> {
        let empty_string = NixValue::String(value::NixString::from(gc_handle.alloc_string("")?));
        Ok(Self {
            true_boolean: gc_handle.alloc(NixValue::Bool(true))?,
            false_boolean: gc_handle.alloc(NixValue::Bool(false))?,
            null_value: gc_handle.alloc(NixValue::Null)?,
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
