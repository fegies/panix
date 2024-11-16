use gc::{GcError, GcHandle, GcPointer};
use parser::ast::{Attrset, BasicValue, KnownNixStringContent, Lambda, List, NixExpr, NixString};

use crate::{
    util::BufferPool,
    vm::{
        opcodes::{ExecutionContext, VmOp},
        value::{self, NixValue, Thunk},
    },
};

mod normalize;
#[cfg(test)]
mod tests;

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("error when allocating")]
    Gc(#[from] GcError),
}

pub fn translate_expression(
    gc_handle: &mut GcHandle,
    mut expr: NixExpr<'_>,
) -> Result<Thunk, CompileError> {
    normalize::normalize_ast(&mut expr);
    let mut key_buffer = Default::default();
    let mut value_buffer = Default::default();
    let mut compiler = Compiler {
        gc_handle,
        key_buffer: &mut key_buffer,
        value_buffer: &mut value_buffer,
    };

    let mut opcode_buf = Vec::new();
    let mut root_scope = LookupScope { parent: None };
    compiler.translate_to_ops(&mut root_scope, &mut opcode_buf, expr)?;

    Ok(Thunk::Deferred {
        context: ExecutionContext {
            entries: gc_handle.alloc_vec(&mut Vec::new())?,
        },
        code: gc_handle.alloc_vec(&mut opcode_buf)?,
    })
}

struct LookupScope<'parent> {
    parent: Option<&'parent LookupScope<'parent>>,
}

struct Compiler<'gc> {
    gc_handle: &'gc mut GcHandle,
    key_buffer: &'gc BufferPool<value::NixString>,
    value_buffer: &'gc BufferPool<Thunk>,
}

impl<'gc> Compiler<'gc> {
    fn alloc_string(
        &mut self,
        str: KnownNixStringContent<'_>,
    ) -> Result<value::NixString, CompileError> {
        let res = match str {
            KnownNixStringContent::Literal(l) => {
                value::NixString::Simple(self.gc_handle.alloc_string(l)?)
            }
            KnownNixStringContent::Composite(_) => todo!(),
            KnownNixStringContent::Empty => value::NixString::Empty,
        };
        Ok(res)
    }

    fn translate_to_ops(
        &mut self,
        lookup_scope: &mut LookupScope,
        target_buffer: &mut Vec<VmOp>,
        expr: NixExpr,
    ) -> Result<(), CompileError> {
        match expr.content {
            parser::ast::NixExprContent::BasicValue(b) => {
                self.translate_basic_value(lookup_scope, target_buffer, b)
            }
            parser::ast::NixExprContent::CompoundValue(_) => todo!(),
            parser::ast::NixExprContent::Code(c) => {
                self.translate_code(lookup_scope, target_buffer, c)
            }
        }
    }

    fn translate_basic_value(
        &mut self,
        lookup_scope: &mut LookupScope<'_>,
        target_buffer: &mut Vec<VmOp>,
        value: BasicValue<'_>,
    ) -> Result<(), CompileError> {
        let value = match value {
            BasicValue::Bool(b) => NixValue::Bool(b),
            BasicValue::Null => NixValue::Null,
            BasicValue::Int(i) => NixValue::Int(i),
            BasicValue::Float(f) => NixValue::Float(f),
            BasicValue::Path(p) => {
                self.translate_string_value(lookup_scope, target_buffer, p)?;
                target_buffer.push(VmOp::CastToPath);
                return Ok(());
            }
            BasicValue::String(s) => {
                return self.translate_string_value(lookup_scope, target_buffer, s);
            }
        };
        target_buffer.push(VmOp::PushImmediate(self.gc_handle.alloc(value)?));
        Ok(())
    }

    fn translate_string_value(
        &mut self,
        lookup_scope: &mut LookupScope<'_>,
        target_buffer: &mut Vec<VmOp>,
        s: NixString<'_>,
    ) -> Result<(), CompileError> {
        match s.content {
            parser::ast::NixStringContent::Known(known) => {
                let literal = NixValue::String(self.alloc_string(known)?);
                let op = VmOp::PushImmediate(self.gc_handle.alloc(literal)?);
                target_buffer.push(op);
            }
            parser::ast::NixStringContent::Interpolated(_) => todo!(),
        }
        Ok(())
    }

    fn translate_code(
        &mut self,
        lookup_scope: &mut LookupScope<'_>,
        target_buffer: &mut Vec<VmOp>,
        code: parser::ast::Code<'_>,
    ) -> Result<(), CompileError> {
        match code {
            parser::ast::Code::LetInExpr(_) => todo!(),
            parser::ast::Code::ValueReference { ident } => todo!(),
            parser::ast::Code::WithExpr(_) => todo!(),
            parser::ast::Code::Lambda(_) => todo!(),
            parser::ast::Code::Op(op) => self.translate_op(lookup_scope, target_buffer, op),
            parser::ast::Code::IfExpr(_) => todo!(),
        }
    }

    fn translate_op(
        &mut self,
        lookup_scope: &mut LookupScope,
        target_buffer: &mut Vec<VmOp>,
        op: parser::ast::Op<'_>,
    ) -> Result<(), CompileError> {
        match op {
            parser::ast::Op::AttrRef {
                left,
                name,
                default,
            } => todo!(),
            parser::ast::Op::Call { function, arg } => todo!(),
            parser::ast::Op::Binop {
                left,
                right,
                opcode,
            } => {
                self.translate_to_ops(lookup_scope, target_buffer, *left)?;
                self.translate_to_ops(lookup_scope, target_buffer, *right)?;
                target_buffer.push(VmOp::Binop(opcode));
            }
            parser::ast::Op::HasAttr { left, path } => todo!(),
            parser::ast::Op::Monop { opcode, body } => todo!(),
        }
        Ok(())
    }
}
