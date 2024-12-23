use std::{collections::BTreeMap, process::id};

use gc::{specialized_types::array::Array, GcError, GcHandle, GcPointer};
use parser::ast::{
    self, BasicValue, IfExpr, KnownNixStringContent, Lambda, NixExpr, SourcePosition,
};

use crate::{
    compiler::{get_null_expr, lookup_scope::LocalThunkRef},
    vm::{
        opcodes::{
            CompareMode, ContextReference, ExecutionContext, LambdaAllocArgs, LambdaCallType,
            ThunkAllocArgs, ValueSource, VmOp,
        },
        value::{self, NixValue, Thunk},
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
                entries: self.compiler.gc_handle.alloc_slice(&[])?,
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
                self.translate_compound_value(lookup_scope, target_buffer, c, expr.position)
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
            parser::ast::Code::WithExpr(_) => {
                unreachable!("With expressions should have been removed by an ast pass")
            }
            parser::ast::Code::Lambda(lambda) => {
                self.translate_lambda(lookup_scope, target_buffer, lambda)
            }
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

    fn translate_lambda(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        lambda: Lambda<'src>,
    ) -> Result<(), CompileError> {
        let mut subscope = lookup_scope.subscope();
        let mut code_buf = Vec::new();

        let mut current_thunk_stack_height = 1; // the lambda always starts with the arg on
                                                // the thunk stack.

        let call_requirements = match lambda.args {
            ast::LambdaArgs::AttrsetBinding { total_name, args } => {
                // if the total name was not provided, we define it as a string that is
                // not actually a valid identifier in the nix language to avoid conflicts.
                let total_name = total_name.unwrap_or("<arg>");
                subscope.push_local_thunkref(total_name, LocalThunkRef(0));

                for (key, default_value) in &args.bindings {
                    subscope.push_local_thunkref(key, LocalThunkRef(current_thunk_stack_height));
                    current_thunk_stack_height += 1;
                }

                let mut call_key_buffer = Vec::new();

                // we don't count the initial argument for the purposes of this
                let added_keys = current_thunk_stack_height - 1;
                if added_keys > 0 {
                    code_buf.push(VmOp::PushBlackholes(added_keys));
                    let first_item_ctx = self
                        .compiler
                        .gc_handle
                        .alloc_slice(&[ValueSource::ThunkStackRef(0)])?;

                    let mut subcode_buf = Vec::new();
                    let mut current_slot = added_keys;
                    for (key, default_value) in args.bindings {
                        let key_ptr: value::NixString =
                            self.compiler.gc_handle.alloc_string(key)?.into();
                        call_key_buffer.push((key_ptr.clone(), default_value.is_none()));
                        let key_ptr = self.compiler.gc_handle.alloc(NixValue::String(key_ptr))?;
                        subcode_buf.push(VmOp::PushImmediate(key_ptr));

                        let mut context_cache = BTreeMap::new();
                        let args = if let Some(default_value) = default_value {
                            // we do have a default, we need to use the more complicated form

                            let mut subscope = subscope.subscope();
                            // resolve the arg0 at least once so that it is included in the context
                            //  even if no code explicitly refers to it
                            subscope.deref_ident(total_name);

                            subcode_buf.push(VmOp::LoadContext(ContextReference(0)));
                            subcode_buf.push(VmOp::GetAttribute { push_error: true });
                            let skip_idx = subcode_buf.len();
                            subcode_buf.push(VmOp::SkipUnless(0));
                            self.translate_to_ops(&mut subscope, &mut subcode_buf, default_value)?;
                            let added_ops = subcode_buf.len() - skip_idx - 1;
                            subcode_buf[skip_idx] = VmOp::SkipUnless(added_ops as u32);
                            let (ctx_id, ctx_insn) = resolve_possibly_cached_context(
                                &mut self.compiler.gc_handle,
                                &mut context_cache,
                                subscope.get_inherit_context(),
                            )?;

                            let code = self.compiler.gc_handle.alloc_vec(&mut subcode_buf)?;

                            self.compiler.gc_handle.alloc(ThunkAllocArgs {
                                code,
                                context_id: ctx_id,
                                context_build_instructions: ctx_insn,
                            })?
                        } else {
                            // no default, the thunk is just a normal getattr
                            subcode_buf.push(VmOp::LoadContext(ContextReference(0)));
                            subcode_buf.push(VmOp::GetAttribute { push_error: false });
                            let code = self.compiler.gc_handle.alloc_vec(&mut subcode_buf)?;
                            self.compiler.gc_handle.alloc(ThunkAllocArgs {
                                code,
                                context_id: 0,
                                context_build_instructions: first_item_ctx.clone(),
                            })?
                        };

                        current_slot -= 1;
                        code_buf.push(VmOp::AllocateThunk {
                            slot: Some(current_slot as u16),
                            args,
                        });
                    }
                }

                LambdaCallType::Attrset {
                    keys: self.compiler.gc_handle.alloc_vec(&mut call_key_buffer)?,
                    includes_rest_pattern: args.includes_rest_pattern,
                }
            }
            ast::LambdaArgs::SimpleBinding(arg_name) => {
                subscope.push_local_thunkref(arg_name, LocalThunkRef(0));

                LambdaCallType::Simple
            }
        };

        let mut body_compiler = ThunkCompiler::new(&mut self.compiler);
        body_compiler.current_thunk_stack_height = current_thunk_stack_height;

        body_compiler.translate_to_ops(&mut subscope, &mut code_buf, *lambda.body)?;
        let code = self.compiler.gc_handle.alloc_vec(&mut code_buf)?;
        let context_build_instructions = self
            .compiler
            .gc_handle
            .alloc_slice(subscope.get_inherit_context())?;

        let args = self.compiler.gc_handle.alloc(LambdaAllocArgs {
            code,
            context_build_instructions,
            call_requirements,
        })?;

        target_buffer.push(VmOp::AllocLambda(args));

        Ok(())
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

                // then the attribute set value
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
                    parser::ast::BinopOpcode::Equals => VmOp::Compare(CompareMode::Equal),
                    parser::ast::BinopOpcode::NotEqual => VmOp::Compare(CompareMode::NotEqual),
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
                    parser::ast::BinopOpcode::LessThanOrEqual => {
                        VmOp::Compare(CompareMode::LessThanOrEqual)
                    }
                    parser::ast::BinopOpcode::LessThanStrict => {
                        VmOp::Compare(CompareMode::LessThanStrict)
                    }
                    parser::ast::BinopOpcode::GreaterOrEqual => {
                        VmOp::Compare(CompareMode::GreaterOrEqual)
                    }
                    parser::ast::BinopOpcode::GreaterThanStrict => {
                        VmOp::Compare(CompareMode::GreaterThanStrict)
                    }
                };
                self.translate_to_ops(lookup_scope, target_buffer, *right)?;
                target_buffer.push(vmop);
            }
            parser::ast::Op::HasAttr { left, path } => match path {
                ast::AttrsetKey::Single(name) => {
                    self.translate_string_value(lookup_scope, target_buffer, name)?;
                    self.translate_to_ops(lookup_scope, target_buffer, *left)?;
                    target_buffer.push(VmOp::HasAttribute);
                }
                ast::AttrsetKey::Multi(_) => {
                    unreachable!("multipart hasattr should have been removed by an ast pass")
                }
            },
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
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        match value {
            parser::ast::CompoundValue::Attrset(attrset) => {
                self.translate_attrset(lookup_scope, target_buffer, attrset, pos)
            }
            parser::ast::CompoundValue::List(parser::ast::List { entries }) => {
                self.translate_list(lookup_scope, target_buffer, entries)
            }
        }
    }

    fn translate_attrset(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        mut attrset: ast::Attrset<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        if attrset.is_recursive {
            unreachable!("recursive attrsets should have been removed by an ast pass");
        }

        let mut context_cache = BTreeMap::new();
        let mut sub_code_buf = Vec::new();

        let previous_height = self.current_thunk_stack_height;
        let mut number_of_keys = 0;

        if !attrset.inherit_keys.is_empty() {
            // first, ensure that we have the inherit source attrsets pushed.
            // we leave the Option filled, but put a null expr in so we can tell later that it had a
            // source expr.
            for entry in &mut attrset.inherit_keys {
                if let Some(src) = entry.source.as_mut() {
                    let src = core::mem::replace(src.as_mut(), get_null_expr());
                    let args = self.compile_subchunk(
                        lookup_scope,
                        &mut sub_code_buf,
                        &mut context_cache,
                        src,
                    )?;
                    target_buffer.push(VmOp::AllocateThunk { slot: None, args });
                    self.current_thunk_stack_height += 1;
                }
            }

            let mut current_inherit_source_idx = previous_height;
            sub_code_buf.push(VmOp::LoadContext(ContextReference(0)));
            let inherit_from_0_code = self.compiler.gc_handle.alloc_vec(&mut sub_code_buf)?;

            // and now, actually emit all the inherit entries.
            for entry in attrset.inherit_keys {
                if entry.source.is_some() {
                    // we actually already emitted the inherit info.
                    // actually loading the value is implemented as a getattr call.
                    let context_insn = self
                        .compiler
                        .gc_handle
                        .alloc_slice(&[ValueSource::ThunkStackRef(current_inherit_source_idx)])?;
                    let context_id = current_inherit_source_idx;
                    current_inherit_source_idx += 1;
                    for ident in entry.entries {
                        let ident_value = self
                            .compiler
                            .alloc_string(KnownNixStringContent::Literal(&ident))?;

                        sub_code_buf.push(VmOp::PushImmediate(ident_value.clone()));
                        // the source, added to the thunk context
                        sub_code_buf.push(VmOp::LoadContext(ContextReference(0)));
                        sub_code_buf.push(VmOp::GetAttribute { push_error: false });

                        let code = self.compiler.gc_handle.alloc_vec(&mut sub_code_buf)?;
                        let alloc_args = self.compiler.gc_handle.alloc(ThunkAllocArgs {
                            context_id,
                            code,
                            context_build_instructions: context_insn.clone(),
                        })?;

                        target_buffer.push(VmOp::PushImmediate(ident_value));
                        target_buffer.push(VmOp::AllocateThunk {
                            slot: None,
                            args: alloc_args,
                        });
                        self.current_thunk_stack_height += 1;
                        number_of_keys += 1;
                    }
                } else {
                    // we are inheriting from the local scope.
                    for ident in entry.entries {
                        let ident_value = self
                            .compiler
                            .alloc_string(KnownNixStringContent::Literal(ident))?;
                        let value_source =
                            lookup_scope
                                .deref_ident(ident)
                                .ok_or_else(|| CompileError::Deref {
                                    value: ident.to_string(),
                                    pos,
                                })?;
                        let context_build_instructions =
                            self.compiler.gc_handle.alloc_slice(&[value_source])?;
                        let alloc_args = self.compiler.gc_handle.alloc(ThunkAllocArgs {
                            code: inherit_from_0_code.clone(),
                            context_id: 0,
                            context_build_instructions,
                        })?;

                        target_buffer.push(VmOp::PushImmediate(ident_value));
                        target_buffer.push(VmOp::AllocateThunk {
                            slot: None,
                            args: alloc_args,
                        });
                        self.current_thunk_stack_height += 1;
                        number_of_keys += 1;
                    }
                }
            }
        }

        // inherit entries are done, emit the normal key-value pairs
        for (key, value) in attrset.attrs {
            let key = match key {
                ast::AttrsetKey::Single(key) => key,
                ast::AttrsetKey::Multi(_) => {
                    unreachable!("mutlipart attrsets shold have been removed by an ast pass.")
                }
            };

            let args =
                self.compile_subchunk(lookup_scope, &mut sub_code_buf, &mut context_cache, value)?;
            target_buffer.push(VmOp::AllocateThunk { slot: None, args });
            self.current_thunk_stack_height += 1;

            self.translate_string_value(lookup_scope, target_buffer, key)?;
            number_of_keys += 1;
        }

        target_buffer.push(VmOp::BuildAttrset(number_of_keys));
        self.current_thunk_stack_height -= number_of_keys;

        let thunks_to_drop = self.current_thunk_stack_height - previous_height;
        if thunks_to_drop > 0 {
            target_buffer.push(VmOp::DropThunks(thunks_to_drop));
            self.current_thunk_stack_height = previous_height;
        }

        Ok(())
    }

    fn translate_list(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        list: Vec<NixExpr<'src>>,
    ) -> Result<(), CompileError> {
        if list.is_empty() {
            target_buffer.push(VmOp::AllocList(0));
            return Ok(());
        }

        let num_entries = list.len();
        let mut ctx_cache = BTreeMap::new();
        let mut sub_code_buf = Vec::new();
        for expr in list {
            let args =
                self.compile_subchunk(lookup_scope, &mut sub_code_buf, &mut ctx_cache, expr)?;
            target_buffer.push(VmOp::AllocateThunk { slot: None, args });
        }
        target_buffer.push(VmOp::AllocList(num_entries as u32));

        Ok(())
    }

    fn translate_value_ref(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        target_buffer: &mut Vec<VmOp>,
        ident: &'src str,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        let opcode = if let Some(src) = lookup_scope.deref_ident(ident) {
            match src {
                ValueSource::ContextReference(ctxref) => {
                    VmOp::LoadContext(ContextReference(ctxref))
                }
                ValueSource::ThunkStackRef(localref) => VmOp::LoadLocalThunk(localref),
            }
        } else {
            // whatever we tried to look up did not exist in the defined scope.
            // try to see if it is a builtin.
            match ident {
                "true" => VmOp::PushImmediate(self.compiler.cached_values.true_boolean.clone()),
                "false" => VmOp::PushImmediate(self.compiler.cached_values.false_boolean.clone()),
                "null" => VmOp::PushImmediate(self.compiler.cached_values.null_value.clone()),
                "throw" => VmOp::PushBuiltin(crate::vm::opcodes::Builtin::Throw),
                "<builtin_hasAttr>" => VmOp::PushBuiltin(crate::vm::opcodes::Builtin::HasAttr),
                "<builtin_abort>" => VmOp::PushBuiltin(crate::vm::opcodes::Builtin::Abort),
                _ => {
                    return Err(CompileError::Deref {
                        value: ident.to_string(),
                        pos,
                    });
                }
            }
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
            resolve_possibly_cached_context(&mut self.compiler.gc_handle, context_cache, ctx_ins)?;
        let res = self.compiler.gc_handle.alloc(ThunkAllocArgs {
            context_id,
            code,
            context_build_instructions,
        })?;

        Ok(res)
    }
}

fn resolve_possibly_cached_context(
    gc_handle: &mut GcHandle,
    context_cache: &mut BTreeMap<Vec<ValueSource>, (u32, GcPointer<Array<ValueSource>>)>,
    ctx_ins: &[ValueSource],
) -> Result<(u32, GcPointer<Array<ValueSource>>), GcError> {
    if let Some((id, cached)) = context_cache.get(ctx_ins) {
        Ok((*id, cached.clone()))
    } else {
        let insn = gc_handle.alloc_slice(ctx_ins)?;
        let id = context_cache.len() as u32;
        context_cache.insert(ctx_ins.to_vec(), (id, insn.clone()));
        Ok((id, insn))
    }
}
