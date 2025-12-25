use bumpalo::Bump;
use gc::{GcError, GcHandle, GcPointer};
use lookup_scope::ScopeBacking;
use parser::ast::SourcePosition;
use thunk_compiler::translate_to_thunk;

use crate::{
    builtins::NixBuiltins,
    compiler::normalize::normalized_ast::{
        BasicValue, KnownNixStringContent, NixExpr, NixExprContent,
    },
    vm::value::{self, NixValue, Thunk},
};

mod lookup_scope;
mod normalize;
mod thunk_compiler;

fn get_null_expr() -> parser::ast::NixExpr<'static> {
    parser::ast::NixExpr {
        position: SourcePosition { line: 0, column: 0 },
        content: parser::ast::NixExprContent::BasicValue(parser::ast::BasicValue::Null),
    }
}
fn get_normalized_null_expr() -> NixExpr<'static> {
    NixExpr {
        position: SourcePosition { line: 0, column: 0 },
        content: NixExprContent::BasicValue(BasicValue::Null),
    }
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
    expr: parser::ast::NixExpr<'src>,
    bump: &'src Bump,
    source_filename: value::NixString,
    builtins: &NixBuiltins,
) -> Result<Thunk, CompileError> {
    let expr = normalize::normalize_ast(expr, bump);

    let mut compiler = Compiler {
        cached_values: CachedValues::new(gc_handle)?,
        source_filename,
        gc_handle,
        builtins,
    };
    let mut scope_backing = ScopeBacking::new();

    translate_to_thunk(scope_backing.build_scope(), &mut compiler, expr)
}

struct Compiler<'gc, 'builtins> {
    gc_handle: &'gc mut GcHandle,
    cached_values: CachedValues,
    builtins: &'builtins NixBuiltins,
    source_filename: value::NixString,
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

impl<'gc, 'builtins> Compiler<'gc, 'builtins> {
    fn alloc_string(
        &mut self,
        str: KnownNixStringContent<'_>,
    ) -> Result<GcPointer<NixValue>, CompileError> {
        let res = match str {
            KnownNixStringContent::Empty | KnownNixStringContent::Literal("") => {
                return Ok(self.cached_values.empty_string.clone());
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
