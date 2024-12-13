use std::{collections::BTreeMap, u32};

use gc::{specialized_types::array::Array, GcPointer};
use parser::ast::{self, BasicValue, IfExpr, KnownNixStringContent, NixExpr, SourcePosition};

use crate::{
    compiler::lookup_scope::LocalThunkRef,
    vm::{
        opcodes::{ContextReference, ExecutionContext, ThunkAllocArgs, ValueSource, VmOp},
        value::{NixValue, Thunk},
    },
};

use super::{lookup_scope::LookupScope, CompileError, Compiler};

pub struct ThunkCompiler<'compiler, 'gc> {
    compiler: &'compiler mut Compiler<'gc>,
    current_thunk_stack_height: u32,
}

impl<'compiler, 'src, 'gc> ThunkCompiler<'compiler, 'gc> {
    pub fn new(compiler: &'compiler mut Compiler<'gc>) -> Self {
        Self {
            compiler,
            current_thunk_stack_height: 0,
        }
    }
    pub fn translate_to_thunk(
        mut self,
        mut scope: LookupScope<'src, '_>,
        expr: NixExpr<'src>,
    ) -> Result<Thunk, CompileError> {
        let mut opcode_buf = Vec::new();
        self.translate_to_ops(&mut scope, &mut opcode_buf, expr)?;

        Ok(Thunk::Deferred {
            context: ExecutionContext {
                entries: self.compiler.gc_handle.alloc_vec(&mut Vec::new())?,
            },
            code: self.compiler.gc_handle.alloc_vec(&mut opcode_buf)?,
        })
    }

    fn clear(&mut self) {
        self.current_thunk_stack_height = 0;
    }

    fn translate_to_ops(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        expr: NixExpr<'src>,
    ) -> Result<(), CompileError> {
        match expr.content {
            parser::ast::NixExprContent::BasicValue(b) => {
                self.translate_basic_value(lookup_scope, target_buffer, b)
            }
            parser::ast::NixExprContent::CompoundValue(c) => {
                self.translate_compound_value(lookup_scope, target_buffer, c)
            }
            parser::ast::NixExprContent::Code(c) => {
                self.translate_code(lookup_scope, target_buffer, c, expr.position)
            }
        }
    }

    fn translate_basic_value(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        value: BasicValue<'src>,
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
        target_buffer.push(VmOp::PushImmediate(self.compiler.gc_handle.alloc(value)?));
        Ok(())
    }

    fn translate_string_value(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        s: ast::NixString<'src>,
    ) -> Result<(), CompileError> {
        match s.content {
            parser::ast::NixStringContent::Known(known) => {
                let literal = self.compiler.alloc_string(known)?;
                let op = VmOp::PushImmediate(literal);
                target_buffer.push(op);
            }
            parser::ast::NixStringContent::Interpolated(mut entries) => {
                entries.reverse();
                let num_entries = entries.len() as u32;
                for entry in entries {
                    match entry {
                        parser::ast::InterpolationEntry::LiteralPiece(known) => {
                            let value = self
                                .compiler
                                .alloc_string(KnownNixStringContent::Literal(known))?;
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
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        code: parser::ast::Code<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        match code {
            parser::ast::Code::LetInExpr(let_expr) => {
                self.translate_let_expr(lookup_scope, target_buffer, let_expr, pos)
            }
            parser::ast::Code::ValueReference { ident } => {
                self.translate_value_ref(lookup_scope, target_buffer, ident, pos)
            }
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
            parser::ast::Code::AssertExpr(_) => todo!(),
        }
    }

    fn translate_op(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        op: parser::ast::Op<'src>,
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
                        target_buffer.push(VmOp::PushImmediate(
                            self.compiler.cached_values.true_boolean.clone(),
                        ));
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
                            self.compiler.cached_values.false_boolean.clone(),
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
                        target_buffer.push(VmOp::PushImmediate(
                            self.compiler.cached_values.true_boolean.clone(),
                        ));

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
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        value: parser::ast::CompoundValue<'src>,
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
                self.translate_list(lookup_scope, target_buffer, entries)
            }
        }
    }

    fn translate_list(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        list: Vec<NixExpr<'src>>,
    ) -> Result<(), CompileError> {
        todo!()
    }

    fn translate_value_ref(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        ident: &'src str,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        let src = lookup_scope
            .deref_ident(ident)
            .ok_or_else(|| CompileError::Deref {
                value: ident.to_string(),
                pos,
            })?;

        let opcode = match src {
            ValueSource::ContextReference(ctxref) => VmOp::LoadContext(ContextReference(ctxref)),
            ValueSource::ThunkStackRef(localref) => VmOp::LoadLocalThunk(localref),
        };

        target_buffer.push(opcode);

        Ok(())
    }

    fn translate_let_expr(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        let_expr: ast::LetInExpr<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        let height_before = self.current_thunk_stack_height;
        let mut added_keys = 0;

        // first, add the new keys to the lookup scope.
        for inherit_entry in &let_expr.inherit_entries {
            if inherit_entry.source.is_some() {
                self.current_thunk_stack_height += 1;
            }
            for ident in &inherit_entry.entries {
                lookup_scope
                    .push_local_thunkref(ident, LocalThunkRef(self.current_thunk_stack_height));
                self.current_thunk_stack_height += 1;
                added_keys += 1;
            }
        }
        for (ident, _) in &let_expr.bindings {
            lookup_scope.push_local_thunkref(ident, LocalThunkRef(self.current_thunk_stack_height));
            self.current_thunk_stack_height += 1;
            added_keys += 1;
        }

        // At this point, the lookup scope is populated and the stack height increased.
        // we need to make sure that the thunks are available for reference. (for mutually recursive
        // bindings etc).
        // To do that, we preallocate a set of blackholes, before overwriting them
        // with the real thunk definitions.
        let added_thunks = self.current_thunk_stack_height - height_before;
        target_buffer.push(VmOp::PushBlackholes(added_thunks));

        let mut cached_context_instructions = BTreeMap::new();
        let mut instruction_buf = Vec::new();

        let mut slot_id = added_thunks as u16;
        // now, emit all the thunk definitions, ensuring they match up with the keys
        // by iterating in the same order.
        for inherit_entry in let_expr.inherit_entries {
            if let Some(source_expr) = inherit_entry.source {
                // first, set the inherit source thunk....
                let thunk_args = self.compile_subchunk(
                    lookup_scope,
                    &mut instruction_buf,
                    &mut cached_context_instructions,
                    *source_expr,
                )?;
                slot_id -= 1;
                target_buffer.push(VmOp::AllocateThunk {
                    slot: Some(slot_id),
                    args: thunk_args,
                });
            } else {
                // as this expression did not have a source, we would need to pick the entries
                // from the context. Since this is a let expression, they would already
                // have been part of the context, so this does not make any sense.
                unreachable!("unqualified inherits in a let in do not make sense.");
            }

            // and now emit all of the inherit keys as simple attrest refs
            let context_ref = self.current_thunk_stack_height - slot_id as u32 + 1;
            let context_build_instructions = self
                .compiler
                .gc_handle
                .alloc_slice(&[ValueSource::ContextReference(context_ref)])?;

            for ident in inherit_entry.entries {
                instruction_buf.push(VmOp::LoadContext(ContextReference(0)));
                instruction_buf.push(VmOp::PushImmediate(
                    self.compiler
                        .alloc_string(KnownNixStringContent::Literal(&ident))?,
                ));
                instruction_buf.push(VmOp::GetAttribute { push_error: false });
                let code = self.compiler.gc_handle.alloc_vec(&mut instruction_buf)?;
                slot_id -= 1;
                target_buffer.push(VmOp::AllocateThunk {
                    slot: Some(slot_id),
                    args: self.compiler.gc_handle.alloc(ThunkAllocArgs {
                        code,
                        context_id: u32::MAX,
                        context_build_instructions: context_build_instructions.clone(),
                    })?,
                });
            }
        }

        // inherit keys done. Now the normal bindings
        for (ident, body) in let_expr.bindings {
            let args = self.compile_subchunk(
                lookup_scope,
                &mut instruction_buf,
                &mut cached_context_instructions,
                body,
            )?;
            slot_id -= 1;
            target_buffer.push(VmOp::AllocateThunk {
                slot: Some(slot_id),
                args,
            });
        }

        // finally, all binding thunks are properly set.
        // now we can emit the body instructions
        self.translate_to_ops(lookup_scope, target_buffer, *let_expr.body)?;

        // and restore the previous status quo
        target_buffer.push(VmOp::DropThunks(added_thunks));
        self.current_thunk_stack_height = height_before;
        lookup_scope.drop_entries(added_keys);

        Ok(())
    }

    fn compile_subchunk(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        opcode_buf: &mut Vec<VmOp>,
        context_cache: &mut BTreeMap<Vec<ValueSource>, (u32, GcPointer<Array<ValueSource>>)>,
        expr: NixExpr<'src>,
    ) -> Result<GcPointer<ThunkAllocArgs>, CompileError> {
        let mut subscope = lookup_scope.subscope();
        ThunkCompiler::new(&mut self.compiler).translate_to_ops(&mut subscope, opcode_buf, expr)?;
        let code = self.compiler.gc_handle.alloc_vec(opcode_buf)?;
        let ctx_ins = subscope.get_inherit_context();
        let (context_id, context_build_instructions) =
            if let Some((id, cached)) = context_cache.get(ctx_ins) {
                (*id, cached.clone())
            } else {
                let insn = self.compiler.gc_handle.alloc_slice(ctx_ins)?;
                let id = context_cache.len() as u32;
                context_cache.insert(ctx_ins.to_vec(), (id, insn.clone()));
                (id, insn)
            };

        let res = self.compiler.gc_handle.alloc(ThunkAllocArgs {
            context_id,
            code,
            context_build_instructions,
        })?;

        Ok(res)
    }
}
