use gc::{GcError, GcHandle, GcPointer};
use parser::ast::{BasicValue, IfExpr, KnownNixStringContent, NixExpr, NixString};

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
        cached_values: CachedValues::new(gc_handle)?,
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
            parser::ast::NixExprContent::CompoundValue(c) => {
                self.translate_compound_value(lookup_scope, target_buffer, c)
            }
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
                let literal = self.alloc_string(known)?;
                let op = VmOp::PushImmediate(literal);
                target_buffer.push(op);
            }
            parser::ast::NixStringContent::Interpolated(mut entries) => {
                entries.reverse();
                let num_entries = entries.len() as u32;
                for entry in entries {
                    match entry {
                        parser::ast::InterpolationEntry::LiteralPiece(known) => {
                            let value = self.alloc_string(KnownNixStringContent::Literal(known))?;
                            target_buffer.push(VmOp::PushImmediate(value));
                        }
                        parser::ast::InterpolationEntry::Expression(expr) => {
                            self.translate_to_ops(lookup_scope, target_buffer, expr)?;
                        }
                    }
                }
                target_buffer.push(VmOp::ConcatStrings(num_entries));
            }
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
            parser::ast::Code::IfExpr(IfExpr {
                condition,
                truthy_case,
                falsy_case,
            }) => {
                self.translate_to_ops(lookup_scope, target_buffer, *condition)?;

                let condition_idx = target_buffer.len();
                target_buffer.push(VmOp::SkipUnless(0));

                self.translate_to_ops(lookup_scope, target_buffer, *truthy_case)?;

                let skip_idx = target_buffer.len();
                target_buffer.push(VmOp::Skip(0));

                // fix up the jump over the true case branch
                let true_branch_ops = target_buffer.len() - condition_idx - 1;
                target_buffer[condition_idx] = VmOp::SkipUnless(true_branch_ops as u32);

                // and now generate the false branch
                self.translate_to_ops(lookup_scope, target_buffer, *falsy_case)?;

                // finally, fix up the unconditional jump at the end of the true case
                let false_branch_ops = target_buffer.len() - skip_idx - 1;
                target_buffer[skip_idx] = VmOp::Skip(false_branch_ops as u32);

                Ok(())
            }
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
            } => {
                // first push the name
                self.translate_string_value(lookup_scope, target_buffer, name)?;

                // then the attribute name
                self.translate_to_ops(lookup_scope, target_buffer, *left)?;

                if let Some(default) = default {
                    // case with default, we need to emit an extra conditional jump
                    // to ensure that the default value is evaluated only if the attribute was not
                    // present
                    target_buffer.push(VmOp::GetAttribute { push_error: true });
                    let skip_idx = target_buffer.len();
                    target_buffer.push(VmOp::SkipUnless(0));
                    self.translate_to_ops(lookup_scope, target_buffer, *default)?;
                    let default_value_ops = target_buffer.len() - skip_idx - 1;
                    target_buffer[skip_idx] = VmOp::SkipUnless(default_value_ops as u32);
                } else {
                    // straightforward, throwing case
                    target_buffer.push(VmOp::GetAttribute { push_error: false });
                }
            }
            parser::ast::Op::Call { function, arg } => {
                self.translate_to_ops(lookup_scope, target_buffer, *function)?;
                self.translate_to_ops(lookup_scope, target_buffer, *arg)?;
                target_buffer.push(VmOp::Call);
            }
            parser::ast::Op::Binop {
                left,
                right,
                opcode,
            } => {
                self.translate_to_ops(lookup_scope, target_buffer, *left)?;
                let vmop = match opcode {
                    parser::ast::BinopOpcode::Add => VmOp::Add,
                    parser::ast::BinopOpcode::ListConcat => VmOp::ConcatLists(2),
                    parser::ast::BinopOpcode::AttrsetMerge => VmOp::MergeAttrsets,
                    parser::ast::BinopOpcode::Equals => VmOp::CompareEqual,
                    parser::ast::BinopOpcode::NotEqual => VmOp::CompareNotEqual,
                    parser::ast::BinopOpcode::Subtract => VmOp::Sub,
                    parser::ast::BinopOpcode::Multiply => VmOp::Mul,
                    parser::ast::BinopOpcode::Divide => VmOp::Div,
                    // these 3 are lazy in the second argument. We emulate that
                    // with a conditional jump
                    parser::ast::BinopOpcode::LogicalOr => {
                        target_buffer.push(VmOp::SkipUnless(2));
                        // true branch
                        target_buffer
                            .push(VmOp::PushImmediate(self.cached_values.true_boolean.clone()));
                        let skip_idx = target_buffer.len();
                        target_buffer.push(VmOp::Skip(0));

                        // false branch
                        self.translate_to_ops(lookup_scope, target_buffer, *right)?;

                        // and fix up the value in the skip at the end of the true branch
                        let false_branch_ops = target_buffer.len() - skip_idx - 1;
                        target_buffer[skip_idx] = VmOp::Skip(false_branch_ops as u32);

                        return Ok(());
                    }
                    parser::ast::BinopOpcode::LogicalAnd => {
                        let condition_idx = target_buffer.len();
                        target_buffer.push(VmOp::SkipUnless(0));

                        // true branch, evaluate rhs
                        self.translate_to_ops(lookup_scope, target_buffer, *right)?;
                        // and skip the false branch
                        target_buffer.push(VmOp::Skip(1));

                        let true_branch_ops = target_buffer.len() - condition_idx - 1;
                        target_buffer[condition_idx] = VmOp::SkipUnless(true_branch_ops as u32);

                        // false branch, just push one false on the stack
                        target_buffer.push(VmOp::PushImmediate(
                            self.cached_values.false_boolean.clone(),
                        ));

                        return Ok(());
                    }
                    parser::ast::BinopOpcode::LogicalImplication => {
                        // an implication can be written as !a || b

                        let condition_idx = target_buffer.len();
                        target_buffer.push(VmOp::SkipUnless(0));

                        // true branch, evaluate rhs
                        self.translate_to_ops(lookup_scope, target_buffer, *right)?;
                        // and skip the false branch
                        target_buffer.push(VmOp::Skip(1));

                        let true_branch_ops = target_buffer.len() - condition_idx - 1;
                        target_buffer[condition_idx] = VmOp::SkipUnless(true_branch_ops as u32);

                        // false branch, just push one false on the stack
                        target_buffer
                            .push(VmOp::PushImmediate(self.cached_values.true_boolean.clone()));

                        return Ok(());
                    }
                    parser::ast::BinopOpcode::LessThanOrEqual => todo!(),
                    parser::ast::BinopOpcode::LessThanStrict => todo!(),
                    parser::ast::BinopOpcode::GreaterOrEqual => todo!(),
                    parser::ast::BinopOpcode::GreaterThanStrict => todo!(),
                };
                self.translate_to_ops(lookup_scope, target_buffer, *right)?;
                target_buffer.push(vmop);
            }
            parser::ast::Op::HasAttr { left, path } => todo!(),
            parser::ast::Op::Monop { opcode, body } => {
                let opcode = match opcode {
                    parser::ast::MonopOpcode::NumericMinus => VmOp::NumericNegate,
                    parser::ast::MonopOpcode::BinaryNot => VmOp::BinaryNot,
                };
                self.translate_to_ops(lookup_scope, target_buffer, *body)?;
                target_buffer.push(opcode);
            }
        }
        Ok(())
    }

    fn translate_compound_value(
        &mut self,
        lookup_scope: &mut LookupScope<'_>,
        target_buffer: &mut Vec<VmOp>,
        value: parser::ast::CompoundValue<'_>,
    ) -> Result<(), CompileError> {
        match value {
            parser::ast::CompoundValue::Attrset(parser::ast::Attrset {
                is_recursive,
                inherit_keys,
                attrs,
            }) => {
                if is_recursive {
                    unreachable!("recursive attrsets should have been normalized by an ast pass")
                }
                todo!()
            }
            parser::ast::CompoundValue::List(parser::ast::List { entries }) => {
                todo!()
            }
        }
        Ok(())
    }
}
