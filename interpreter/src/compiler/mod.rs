use std::collections::BTreeMap;

use gc::{
    specialized_types::{array::Array, string::SimpleGcString},
    GcError, GcHandle, GcPointer, GcString,
};
use parser::ast::{Code, MonopOpcode, SourcePosition};

use self::ast::{ExecutionContext, Thunk};

pub mod ast;

#[cfg(test)]
mod tests;

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("error when allocating")]
    Gc(#[from] GcError),
}

struct Scope<'c, 'gc, 'ast> {
    idents: BTreeMap<&'ast str, GcPointer<Thunk>>,
    compiler: &'c mut Compiler<'gc, 'ast>,
    next_level: Option<&'c Scope<'c, 'gc, 'ast>>,
}

pub struct Compiler<'gc, 'ast> {
    ident_set: BTreeMap<&'ast str, GcString>,
    output_buffer: Vec<ast::VmOp>,
    gc_handle: &'gc mut GcHandle,
}

impl<'gc, 'ast> Compiler<'gc, 'ast> {
    pub fn new(gc_handle: &'gc mut GcHandle) -> Self {
        Self {
            ident_set: BTreeMap::new(),
            output_buffer: Vec::new(),
            gc_handle,
        }
    }

    pub fn translate_expression(
        &mut self,
        expr: parser::ast::NixExpr<'ast>,
    ) -> Result<Thunk, CompileError> {
        Scope {
            idents: BTreeMap::new(),
            compiler: self,
            next_level: None,
        }
        .translate_expression(expr)?;

        let entries = self.gc_handle.alloc_vec(&mut Vec::new())?;
        let code = self.gc_handle.alloc_vec(&mut self.output_buffer)?;
        Ok(Thunk::Deferred {
            context: ExecutionContext { entries },
            code,
        })
    }
}

impl<'compiler, 'gc, 'ast> Scope<'compiler, 'gc, 'ast> {
    pub fn translate_expression(
        &mut self,
        expr: parser::ast::NixExpr<'ast>,
    ) -> Result<(), CompileError> {
        match expr.content {
            parser::ast::NixExprContent::BasicValue(_) => todo!(),
            parser::ast::NixExprContent::CompoundValue(_) => todo!(),
            parser::ast::NixExprContent::Code(code) => self.translate_code(code, expr.position)?,
        };
        Ok(())
    }

    pub fn translate_code(
        &mut self,
        code: Code<'ast>,
        position: SourcePosition,
    ) -> Result<(), CompileError> {
        match code {
            Code::LetInExpr(_) => todo!(),
            Code::RecAttrset => todo!(),
            Code::ValueReference { ident } => todo!(),
            Code::WithExpr(_) => todo!(),
            Code::Lambda(l) => todo!(),
            Code::Op(op) => self.translate_op(op, position),
            Code::IfExpr(_) => todo!(),
        }
    }

    fn translate_op(
        &mut self,
        op: parser::ast::Op<'ast>,
        position: SourcePosition,
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
            } => todo!(),
            parser::ast::Op::HasAttr { left, path } => todo!(),
            parser::ast::Op::Monop { opcode, body } => {
                match opcode {
                    MonopOpcode::NumericMinus => todo!(),
                    MonopOpcode::BinaryNot => {
                        self.translate_expression(*body)?;
                        self.compiler.output_buffer.push(ast::VmOp::BinaryNot);
                    }
                };
                Ok(())
            }
        }
    }
}
