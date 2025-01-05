use std::collections::BTreeMap;

use gc::{specialized_types::array::Array, GcError, GcHandle, GcPointer};
use parser::ast::{
    self, BasicValue, IfExpr, KnownNixStringContent, Lambda, NixExpr, SourcePosition,
};

use crate::{
    builtins::Builtins,
    compiler::{get_null_expr, lookup_scope::LocalThunkRef},
    vm::{
        opcodes::{
            CompareMode, ExecutionContext, LambdaAllocArgs, LambdaCallType, ThunkAllocArgs,
            ValueSource, VmOp,
        },
        value::{self, NixValue, Thunk},
    },
};

use super::{lookup_scope::LookupScope, CompileError, Compiler};
pub use opcode_buf::OpcodeBuf;

mod opcode_buf {
    use gc::GcResult;

    use crate::vm::opcodes;

    use super::*;

    #[derive(Default)]
    pub struct OpcodeBuf {
        opcodes: Vec<VmOp>,
        positions: Vec<opcodes::SourcePosition>,
    }

    impl OpcodeBuf {
        pub fn push(&mut self, op: VmOp, pos: impl Into<opcodes::SourcePosition>) {
            self.opcodes.push(op);
            self.positions.push(pos.into());
        }

        pub fn clear(&mut self) {
            self.opcodes.clear();
            self.positions.clear();
        }

        pub fn freeze(
            &mut self,
            gc: &mut GcHandle,
        ) -> GcResult<(
            GcPointer<Array<VmOp>>,
            GcPointer<Array<opcodes::SourcePosition>>,
        )> {
            let op_arr = gc.alloc_vec(&mut self.opcodes)?;

            // optimize our bytecode, inserting tailcalls where possible before
            // emitting the result
            insert_tailcalls(&mut self.opcodes);

            let pos_arr = gc.alloc_vec(&mut self.positions)?;
            Ok((op_arr, pos_arr))
        }

        pub fn len(&self) -> usize {
            self.opcodes.len()
        }
    }

    impl core::ops::Index<usize> for OpcodeBuf {
        type Output = VmOp;

        fn index(&self, index: usize) -> &Self::Output {
            &self.opcodes[index]
        }
    }
    impl core::ops::IndexMut<usize> for OpcodeBuf {
        fn index_mut(&mut self, index: usize) -> &mut Self::Output {
            &mut self.opcodes[index]
        }
    }
    impl AsRef<[VmOp]> for OpcodeBuf {
        fn as_ref(&self) -> &[VmOp] {
            &self.opcodes
        }
    }
    impl AsMut<[VmOp]> for OpcodeBuf {
        fn as_mut(&mut self) -> &mut [VmOp] {
            &mut self.opcodes
        }
    }
}

struct ThunkCompiler<'compiler, 'gc, 'builtins, 'buffer> {
    compiler: &'compiler mut Compiler<'gc, 'builtins>,
    current_thunk_stack_height: u32,
    opcode_buf: &'buffer mut OpcodeBuf,
}

pub fn translate_to_thunk<'src>(
    mut scope: LookupScope<'src, '_>,
    compiler: &mut Compiler,
    expr: NixExpr<'src>,
) -> Result<Thunk, CompileError> {
    let mut opcode_buf = OpcodeBuf::default();
    ThunkCompiler::new(compiler, &mut opcode_buf).translate_to_ops(&mut scope, expr)?;

    let (code, positions) = opcode_buf.freeze(&mut compiler.gc_handle)?;
    Ok(Thunk::Deferred {
        context: ExecutionContext {
            entries: compiler.gc_handle.alloc_slice(&[])?,
            source_filename: compiler.source_filename.clone(),
            source_positions: Some(positions),
        },
        code,
    })
}

impl<'compiler, 'src, 'gc, 'builtins, 'buffer> ThunkCompiler<'compiler, 'gc, 'builtins, 'buffer> {
    fn new(
        compiler: &'compiler mut Compiler<'gc, 'builtins>,
        opcode_buf: &'buffer mut OpcodeBuf,
    ) -> Self {
        Self {
            compiler,
            current_thunk_stack_height: 0,
            opcode_buf,
        }
    }

    fn translate_to_ops(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        expr: NixExpr<'src>,
    ) -> Result<(), CompileError> {
        match expr.content {
            parser::ast::NixExprContent::BasicValue(b) => {
                self.translate_basic_value(lookup_scope, b, expr.position)
            }
            parser::ast::NixExprContent::CompoundValue(c) => {
                self.translate_compound_value(lookup_scope, c, expr.position)
            }
            parser::ast::NixExprContent::Code(c) => {
                self.translate_code(lookup_scope, c, expr.position)
            }
        }
    }

    fn translate_basic_value(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        value: BasicValue<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        let value = match value {
            BasicValue::Bool(b) => NixValue::Bool(b),
            BasicValue::Null => NixValue::Null,
            BasicValue::Int(i) => NixValue::Int(i),
            BasicValue::Float(f) => NixValue::Float(f),
            BasicValue::Path(p) => {
                self.translate_string_value(lookup_scope, p)?;
                self.opcode_buf.push(
                    VmOp::CastToPath {
                        source_location: self.compiler.source_filename.clone(),
                    },
                    pos,
                );
                return Ok(());
            }
            BasicValue::String(s) => {
                return self.translate_string_value(lookup_scope, s);
            }
            BasicValue::SearchPath(_) => {
                unreachable!("search paths should have been removed by an ast pass")
            }
        };
        self.opcode_buf.push(
            VmOp::PushImmediate(self.compiler.gc_handle.alloc(value)?),
            pos,
        );
        Ok(())
    }

    fn translate_string_value(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        s: ast::NixString<'src>,
    ) -> Result<(), CompileError> {
        match s.content {
            parser::ast::NixStringContent::Known(known) => {
                let literal = self.compiler.alloc_string(known)?;
                let op = VmOp::PushImmediate(literal);
                self.opcode_buf.push(op, s.position);
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
                            self.opcode_buf.push(VmOp::PushImmediate(value), s.position);
                        }
                        parser::ast::InterpolationEntry::Expression(expr) => {
                            self.translate_to_ops(lookup_scope, expr)?;
                        }
                    }
                }
                self.opcode_buf
                    .push(VmOp::ConcatStrings(num_entries), s.position);
            }
        }
        Ok(())
    }

    fn translate_code(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        code: parser::ast::Code<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        match code {
            parser::ast::Code::LetInExpr(let_expr) => {
                self.translate_let_expr(lookup_scope, let_expr, pos)
            }
            parser::ast::Code::ValueReference { ident } => {
                self.translate_value_ref(lookup_scope, ident, pos)
            }
            parser::ast::Code::WithExpr(_) => {
                unreachable!("With expressions should have been removed by an ast pass")
            }
            parser::ast::Code::Lambda(lambda) => self.translate_lambda(lookup_scope, lambda, pos),
            parser::ast::Code::Op(op) => self.translate_op(lookup_scope, op, pos),
            parser::ast::Code::IfExpr(IfExpr {
                condition,
                truthy_case,
                falsy_case,
            }) => {
                let if_pos = condition.position;
                self.translate_to_ops(lookup_scope, *condition)?;

                let condition_idx = self.opcode_buf.len();
                self.opcode_buf.push(VmOp::SkipUnless(0), if_pos);

                self.translate_to_ops(lookup_scope, *truthy_case)?;

                let skip_idx = self.opcode_buf.len();
                self.opcode_buf.push(VmOp::Skip(0), if_pos);

                // fix up the jump over the true case branch
                let true_branch_ops = self.opcode_buf.len() - condition_idx - 1;
                self.opcode_buf[condition_idx] = VmOp::SkipUnless(true_branch_ops as u32);

                // and now generate the false branch
                self.translate_to_ops(lookup_scope, *falsy_case)?;

                // finally, fix up the unconditional jump at the end of the true case
                let false_branch_ops = self.opcode_buf.len() - skip_idx - 1;
                self.opcode_buf[skip_idx] = VmOp::Skip(false_branch_ops as u32);

                Ok(())
            }
            parser::ast::Code::AssertExpr(_) => {
                unreachable!("assert should have ben removed by an ast pass")
            }
        }
    }

    fn translate_lambda(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        lambda: Lambda<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        let mut subscope = lookup_scope.subscope();
        let mut code_buf = OpcodeBuf::default();

        let mut current_thunk_stack_height = 1; // the lambda always starts with the arg on
                                                // the thunk stack.

        let call_requirements = match lambda.args {
            ast::LambdaArgs::AttrsetBinding { total_name, args } => {
                // if the total name was not provided, we define it as a string that is
                // not actually a valid identifier in the nix language to avoid conflicts.
                let total_name = total_name.unwrap_or("<arg>");
                subscope.push_local_thunkref(total_name, LocalThunkRef(0));

                for (key, _default_value) in &args.bindings {
                    subscope.push_local_thunkref(key, LocalThunkRef(current_thunk_stack_height));
                    current_thunk_stack_height += 1;
                }

                let mut call_key_buffer = Vec::new();

                // we don't count the initial argument for the purposes of this
                let added_keys = current_thunk_stack_height - 1;
                if added_keys > 0 {
                    code_buf.push(VmOp::PushBlackholes(added_keys), pos);
                    let first_item_ctx = self
                        .compiler
                        .gc_handle
                        .alloc_slice(&[ValueSource::ThunkStackRef(0)])?;

                    let mut subcode_buf = OpcodeBuf::default();
                    let mut current_slot = 1;
                    for (key, default_value) in args.bindings {
                        let key_ptr: value::NixString =
                            self.compiler.gc_handle.alloc_string(key)?.into();
                        call_key_buffer.push((key_ptr.clone(), default_value.is_none()));
                        let key_ptr = self.compiler.gc_handle.alloc(NixValue::String(key_ptr))?;

                        subcode_buf.push(VmOp::PushImmediate(key_ptr), pos);

                        let mut context_cache = BTreeMap::new();
                        let args = if let Some(default_value) = default_value {
                            // we do have a default, we need to use the more complicated form

                            let mut subscope = subscope.subscope();
                            // resolve the arg0 at least once so that it is included in the context
                            //  even if no code explicitly refers to it
                            subscope.deref_ident(total_name);

                            subcode_buf
                                .push(VmOp::LoadThunk(ValueSource::ContextReference(0)), pos);
                            subcode_buf.push(VmOp::GetAttribute { push_error: true }, pos);
                            let skip_idx = subcode_buf.len();
                            subcode_buf.push(VmOp::SkipUnless(0), pos);
                            ThunkCompiler::new(self.compiler, &mut subcode_buf)
                                .translate_to_ops(&mut subscope, default_value)?;
                            let added_ops = subcode_buf.len() - skip_idx - 1;
                            subcode_buf[skip_idx] = VmOp::SkipUnless(added_ops as u32);
                            let (ctx_id, ctx_insn) = resolve_possibly_cached_context(
                                &mut self.compiler.gc_handle,
                                &mut context_cache,
                                subscope.get_inherit_context(),
                            )?;

                            let (code, source_positions) =
                                subcode_buf.freeze(&mut self.compiler.gc_handle)?;

                            self.compiler.gc_handle.alloc(ThunkAllocArgs {
                                code,
                                context_id: ctx_id,
                                context_build_instructions: ctx_insn,
                                source_file: self.compiler.source_filename.clone(),
                                source_positions,
                            })?
                        } else {
                            // no default, the thunk is just a normal getattr
                            subcode_buf
                                .push(VmOp::LoadThunk(ValueSource::ContextReference(0)), pos);
                            subcode_buf.push(VmOp::GetAttribute { push_error: false }, pos);
                            let (code, source_positions) =
                                subcode_buf.freeze(&mut self.compiler.gc_handle)?;
                            self.compiler.gc_handle.alloc(ThunkAllocArgs {
                                code,
                                context_id: 0,
                                context_build_instructions: first_item_ctx.clone(),
                                source_file: self.compiler.source_filename.clone(),
                                source_positions,
                            })?
                        };

                        code_buf.push(VmOp::AllocateThunk(args), pos);
                        code_buf.push(
                            VmOp::OverwriteThunk {
                                stackref: current_slot,
                            },
                            pos,
                        );
                        current_slot += 1;
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

        let mut body_compiler = ThunkCompiler::new(&mut self.compiler, &mut code_buf);
        body_compiler.current_thunk_stack_height = current_thunk_stack_height;
        body_compiler.translate_to_ops(&mut subscope, *lambda.body)?;

        let (code, source_positions) = code_buf.freeze(&mut self.compiler.gc_handle)?;
        let context_build_instructions = self
            .compiler
            .gc_handle
            .alloc_slice(subscope.get_inherit_context())?;

        let args = self.compiler.gc_handle.alloc(LambdaAllocArgs {
            code,
            context_build_instructions,
            call_requirements,
            source_file: self.compiler.source_filename.clone(),
            source_locations: source_positions,
        })?;

        self.opcode_buf.push(VmOp::AllocLambda(args), pos);

        Ok(())
    }

    fn translate_op(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        op: parser::ast::Op<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        match op {
            parser::ast::Op::AttrRef {
                left,
                name,
                default,
            } => {
                // first push the name
                self.translate_string_value(lookup_scope, name)?;

                // then the attribute set value
                self.translate_to_ops(lookup_scope, *left)?;

                if let Some(default) = default {
                    // case with default, we need to emit an extra conditional jump
                    // to ensure that the default value is evaluated only if the attribute was not
                    // present
                    self.opcode_buf
                        .push(VmOp::GetAttribute { push_error: true }, pos);
                    let skip_idx = self.opcode_buf.len();
                    self.opcode_buf.push(VmOp::SkipUnless(0), pos);
                    self.translate_to_ops(lookup_scope, *default)?;
                    let default_value_ops = self.opcode_buf.len() - skip_idx - 1;
                    self.opcode_buf[skip_idx] = VmOp::SkipUnless(default_value_ops as u32);
                } else {
                    // straightforward, throwing case
                    self.opcode_buf
                        .push(VmOp::GetAttribute { push_error: false }, pos);
                }
            }
            parser::ast::Op::Call { function, arg } => {
                self.translate_to_ops(lookup_scope, *function)?;
                // the arg thunk
                let alloc_insn = self.compile_subchunk(
                    lookup_scope,
                    &mut OpcodeBuf::default(),
                    &mut BTreeMap::new(),
                    *arg,
                )?;

                self.opcode_buf.push(alloc_insn, pos);
                self.opcode_buf.push(VmOp::Call, pos);
            }
            parser::ast::Op::Binop {
                left,
                right,
                opcode,
            } => {
                self.translate_to_ops(lookup_scope, *left)?;
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
                        self.opcode_buf.push(VmOp::SkipUnless(2), pos);
                        // true branch
                        self.opcode_buf.push(
                            VmOp::PushImmediate(self.compiler.cached_values.true_boolean.clone()),
                            pos,
                        );
                        let skip_idx = self.opcode_buf.len();
                        self.opcode_buf.push(VmOp::Skip(0), pos);

                        // false branch
                        self.translate_to_ops(lookup_scope, *right)?;

                        // and fix up the value in the skip at the end of the true branch
                        let false_branch_ops = self.opcode_buf.len() - skip_idx - 1;
                        self.opcode_buf[skip_idx] = VmOp::Skip(false_branch_ops as u32);

                        return Ok(());
                    }
                    parser::ast::BinopOpcode::LogicalAnd => {
                        let condition_idx = self.opcode_buf.len();
                        self.opcode_buf.push(VmOp::SkipUnless(0), pos);

                        // true branch, evaluate rhs
                        self.translate_to_ops(lookup_scope, *right)?;
                        // and skip the false branch
                        self.opcode_buf.push(VmOp::Skip(1), pos);

                        let true_branch_ops = self.opcode_buf.len() - condition_idx - 1;
                        self.opcode_buf[condition_idx] = VmOp::SkipUnless(true_branch_ops as u32);

                        // false branch, just push one false on the stack
                        self.opcode_buf.push(
                            VmOp::PushImmediate(self.compiler.cached_values.false_boolean.clone()),
                            pos,
                        );

                        return Ok(());
                    }
                    parser::ast::BinopOpcode::LogicalImplication => {
                        // an implication can be written as !a || b

                        let condition_idx = self.opcode_buf.len();
                        self.opcode_buf.push(VmOp::SkipUnless(0), pos);

                        // true branch, evaluate rhs
                        self.translate_to_ops(lookup_scope, *right)?;
                        // and skip the false branch
                        self.opcode_buf.push(VmOp::Skip(1), pos);

                        let true_branch_ops = self.opcode_buf.len() - condition_idx - 1;
                        self.opcode_buf[condition_idx] = VmOp::SkipUnless(true_branch_ops as u32);

                        // false branch, just push one false on the stack
                        self.opcode_buf.push(
                            VmOp::PushImmediate(self.compiler.cached_values.true_boolean.clone()),
                            pos,
                        );

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
                self.translate_to_ops(lookup_scope, *right)?;
                self.opcode_buf.push(vmop, pos);
            }
            parser::ast::Op::HasAttr { left, path } => match path {
                ast::AttrsetKey::Single(name) => {
                    self.translate_string_value(lookup_scope, name)?;
                    self.translate_to_ops(lookup_scope, *left)?;
                    self.opcode_buf.push(VmOp::HasAttribute, pos);
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
                self.translate_to_ops(lookup_scope, *body)?;
                self.opcode_buf.push(opcode, pos);
            }
        }
        Ok(())
    }

    fn translate_compound_value(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        value: parser::ast::CompoundValue<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        match value {
            parser::ast::CompoundValue::Attrset(attrset) => {
                self.translate_attrset(lookup_scope, attrset, pos)
            }
            parser::ast::CompoundValue::List(parser::ast::List { entries }) => {
                self.translate_list(lookup_scope, entries, pos)
            }
        }
    }

    fn translate_attrset(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        mut attrset: ast::Attrset<'src>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        if attrset.is_recursive {
            unreachable!("recursive attrsets should have been removed by an ast pass");
        }

        let mut context_cache = BTreeMap::new();
        let mut subcode_buf = OpcodeBuf::default();

        let previous_height = self.current_thunk_stack_height;
        let mut number_of_keys = 0;

        if !attrset.inherit_keys.is_empty() {
            // first, ensure that we have the inherit source attrsets pushed.
            // we leave the Option filled, but put a null expr in so we can tell later that it had a
            // source expr.
            for entry in &mut attrset.inherit_keys {
                if let Some(src) = entry.source.as_mut() {
                    let src = core::mem::replace(src.as_mut(), get_null_expr());
                    let push_thunk_op = self.compile_subchunk(
                        lookup_scope,
                        &mut subcode_buf,
                        &mut context_cache,
                        src,
                    )?;
                    self.opcode_buf.push(push_thunk_op, pos);
                    self.current_thunk_stack_height += 1;
                }
            }

            let mut current_inherit_source_idx = previous_height;

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

                        subcode_buf.push(VmOp::PushImmediate(ident_value.clone()), pos);
                        // the source, added to the thunk context
                        subcode_buf.push(VmOp::LoadThunk(ValueSource::ContextReference(0)), pos);
                        subcode_buf.push(VmOp::GetAttribute { push_error: false }, pos);

                        let (code, source_positions) =
                            subcode_buf.freeze(&mut self.compiler.gc_handle)?;
                        let alloc_args = self.compiler.gc_handle.alloc(ThunkAllocArgs {
                            context_id,
                            code,
                            context_build_instructions: context_insn.clone(),
                            source_file: self.compiler.source_filename.clone(),
                            source_positions,
                        })?;

                        self.opcode_buf.push(VmOp::PushImmediate(ident_value), pos);
                        self.opcode_buf.push(VmOp::AllocateThunk(alloc_args), pos);
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

                        self.opcode_buf.push(VmOp::PushImmediate(ident_value), pos);
                        self.opcode_buf
                            .push(VmOp::DuplicateThunk(value_source), pos);
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

            self.translate_string_value(lookup_scope, key)?;
            let push_thunk_op =
                self.compile_subchunk(lookup_scope, &mut subcode_buf, &mut context_cache, value)?;
            self.opcode_buf.push(push_thunk_op, pos);
            self.current_thunk_stack_height += 1;

            number_of_keys += 1;
        }

        self.opcode_buf
            .push(VmOp::BuildAttrset(number_of_keys), pos);
        self.current_thunk_stack_height -= number_of_keys;

        let thunks_to_drop = self.current_thunk_stack_height - previous_height;
        if thunks_to_drop > 0 {
            self.opcode_buf.push(VmOp::DropThunks(thunks_to_drop), pos);
            self.current_thunk_stack_height = previous_height;
        }

        Ok(())
    }

    fn translate_list(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        list: Vec<NixExpr<'src>>,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        if list.is_empty() {
            self.opcode_buf.push(VmOp::AllocList(0), pos);
            return Ok(());
        }

        let num_entries = list.len();
        let mut ctx_cache = BTreeMap::new();
        for expr in list {
            let push_thunk_op = self.compile_subchunk(
                lookup_scope,
                &mut OpcodeBuf::default(),
                &mut ctx_cache,
                expr,
            )?;
            self.opcode_buf.push(push_thunk_op, pos);
        }
        self.opcode_buf
            .push(VmOp::AllocList(num_entries as u32), pos);

        Ok(())
    }

    fn translate_value_ref(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        ident: &'src str,
        pos: SourcePosition,
    ) -> Result<(), CompileError> {
        let opcode = if let Some(src) = lookup_scope.deref_ident(ident) {
            VmOp::LoadThunk(src)
        } else {
            // whatever we tried to look up did not exist in the defined scope.
            // try to see if it is a builtin.
            match ident {
                "true" => VmOp::PushImmediate(self.compiler.cached_values.true_boolean.clone()),
                "false" => VmOp::PushImmediate(self.compiler.cached_values.false_boolean.clone()),
                "null" => VmOp::PushImmediate(self.compiler.cached_values.null_value.clone()),
                _ => {
                    if let Some(builtin) = self.compiler.builtins.get_builtin(ident) {
                        VmOp::PushImmediate(
                            self.compiler.gc_handle.alloc(NixValue::Builtin(builtin))?,
                        )
                    } else {
                        return Err(CompileError::Deref {
                            value: ident.to_string(),
                            pos,
                        });
                    }
                }
            }
        };

        self.opcode_buf.push(opcode, pos);

        Ok(())
    }

    fn translate_let_expr(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
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
        self.opcode_buf
            .push(VmOp::PushBlackholes(added_thunks), pos);

        let mut cached_context_instructions = BTreeMap::new();
        let mut instruction_buf = OpcodeBuf::default();

        let mut slot_to_overwrite = height_before;

        // now, emit all the thunk definitions, ensuring they match up with the keys
        // by iterating in the same order.
        for inherit_entry in let_expr.inherit_entries {
            // as this expression did not have a source, we would need to pick the entries
            // from the context. Since this is a let expression, they would already
            // have been part of the context, so this does not make any sense.
            let source_expr = inherit_entry
                .source
                .expect("unqualified inherites in a let in do not make sense.");

            // first, set the inherit source thunk....
            let create_thunk_op = self.compile_subchunk(
                lookup_scope,
                &mut instruction_buf,
                &mut cached_context_instructions,
                *source_expr,
            )?;
            self.opcode_buf.push(create_thunk_op, pos);
            self.opcode_buf.push(
                VmOp::OverwriteThunk {
                    stackref: slot_to_overwrite,
                },
                pos,
            );

            // and now emit all of the inherit keys as simple attrest refs
            let context_ref = slot_to_overwrite;
            slot_to_overwrite += 1;

            let context_build_instructions = self
                .compiler
                .gc_handle
                .alloc_slice(&[ValueSource::ThunkStackRef(context_ref)])?;

            let inherit_positions =
                self.compiler
                    .gc_handle
                    .alloc_slice(&[pos.into(), pos.into(), pos.into()])?;

            for ident in inherit_entry.entries {
                let ident = self
                    .compiler
                    .alloc_string(KnownNixStringContent::Literal(ident))?;
                let code = self.compiler.gc_handle.alloc_slice(&[
                    VmOp::PushImmediate(ident),
                    VmOp::LoadThunk(ValueSource::ContextReference(0)),
                    VmOp::GetAttribute { push_error: false },
                ])?;

                self.opcode_buf.push(
                    VmOp::AllocateThunk(self.compiler.gc_handle.alloc(ThunkAllocArgs {
                        code,
                        context_id: 0,
                        context_build_instructions: context_build_instructions.clone(),
                        source_file: self.compiler.source_filename.clone(),
                        source_positions: inherit_positions.clone(),
                    })?),
                    pos,
                );
                self.opcode_buf.push(
                    VmOp::OverwriteThunk {
                        stackref: slot_to_overwrite,
                    },
                    pos,
                );
                slot_to_overwrite += 1;
            }
        }

        // inherit keys done. Now the normal bindings
        for (_ident, body) in let_expr.bindings {
            let push_thunk_op = self.compile_subchunk(
                lookup_scope,
                &mut instruction_buf,
                &mut cached_context_instructions,
                body,
            )?;
            self.opcode_buf.push(push_thunk_op, pos);
            self.opcode_buf.push(
                VmOp::OverwriteThunk {
                    stackref: slot_to_overwrite,
                },
                pos,
            );
            slot_to_overwrite += 1;
        }

        // finally, all binding thunks are properly set.
        // now we can emit the body instructions
        self.translate_to_ops(lookup_scope, *let_expr.body)?;

        // and restore the previous status quo
        self.opcode_buf.push(VmOp::DropThunks(added_thunks), pos);
        self.current_thunk_stack_height = height_before;
        lookup_scope.drop_entries(added_keys);

        Ok(())
    }

    /// compiles the provided expression.
    /// returns a vmop that modifies the local thunk stack.
    fn compile_subchunk(
        &mut self,
        lookup_scope: &mut LookupScope<'src, '_>,
        opcode_buf: &mut OpcodeBuf,
        context_cache: &mut BTreeMap<Vec<ValueSource>, (u32, GcPointer<Array<ValueSource>>)>,
        expr: NixExpr<'src>,
    ) -> Result<VmOp, CompileError> {
        let mut subscope = lookup_scope.subscope();
        ThunkCompiler::new(&mut self.compiler, opcode_buf).translate_to_ops(&mut subscope, expr)?;
        let ctx_ins = subscope.get_inherit_context();

        if let (&[VmOp::LoadThunk(ValueSource::ContextReference(0))], &[value_source]) =
            (opcode_buf.as_ref(), ctx_ins)
        {
            // the requested instruction should push a value on the local thunk stack.
            // the compiled thunk only loads a single value from its context, which we would select
            // from the allocating context at the time of thunk allocation.
            //
            // In this case, we can skip the thunk allocation and just directly
            // instruct the interpreter to duplicate the source thunk to the top of the stack
            //
            // Since we are not using anything from the compiled opcodes, we need to ensure
            // that they do not pollute the next execution (for instance in the case of lists)
            opcode_buf.clear();
            return Ok(VmOp::DuplicateThunk(value_source));
        }

        let (code, source_positions) = opcode_buf.freeze(&mut self.compiler.gc_handle)?;
        let (context_id, context_build_instructions) =
            resolve_possibly_cached_context(&mut self.compiler.gc_handle, context_cache, ctx_ins)?;
        let alloc_args = self.compiler.gc_handle.alloc(ThunkAllocArgs {
            context_id,
            code,
            source_positions,
            context_build_instructions,
            source_file: self.compiler.source_filename.clone(),
        })?;

        Ok(VmOp::AllocateThunk(alloc_args))
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

/// tune the emitted bytecode by replacing all call instructions
/// that are followed only by skip and dropThunk instructions
/// with TailCall instructions
fn insert_tailcalls(mut opcodes: &mut [VmOp]) {
    fn is_tailcall_safe(mut opcodes: &[VmOp]) -> bool {
        while let Some(first) = opcodes.first() {
            let advance_count = match first {
                VmOp::DropThunks(_) => 1, // dropThunks instructions are fine because the thunk
                // stack would be implicitly cleared by the tail call.
                VmOp::Skip(n) => *n as usize,
                _ => return false,
            };
            opcodes = &opcodes[advance_count..];
        }

        // we did not find anything unsafe...
        true
    }

    while let Some((opcode, tail)) = opcodes.split_first_mut() {
        if let VmOp::Call = opcode {
            if is_tailcall_safe(tail) {
                *opcode = VmOp::TailCall;
            }
        }
        opcodes = tail;
    }
}
