use std::{cmp::Ordering, rc::Rc};

use gc::{specialized_types::array::Array, GcError, GcHandle, GcPointer};

use crate::{
    builtins::{get_builtins, Builtins, NixBuiltins},
    vm::{
        opcodes::{ExecutionContext, LambdaAllocArgs, SourcePosition, ValueSource, VmOp},
        value::{self, Attrset, Function, List, NixString, NixValue, PathValue, Thunk},
    },
    EvaluateError,
};

pub struct Evaluator<'gc> {
    pub gc_handle: &'gc mut GcHandle,
    stack_cache: Vec<ThunkEvalState>,
    thunk_alloc_buffer: Vec<GcPointer<Thunk>>,
    builtins: Rc<NixBuiltins>,

    error_message_printed: bool,
}
#[derive(Default, Debug)]
struct ThunkEvalState {
    context: Vec<GcPointer<Thunk>>,
    local_stack: Vec<NixValue>,
    thunk_stack: Vec<GcPointer<Thunk>>,
    code_buf: Vec<VmOp>,
    code_source_positions: Option<GcPointer<Array<SourcePosition>>>,
}

struct ThunkEvaluator<'eval, 'gc> {
    state: ThunkEvalState,
    evaluator: &'eval mut Evaluator<'gc>,
    source_filename: NixString,
    insn_counter: usize,

    // a buffer that is filled with the filenames for
    // tail recursion calls to allow us to recover the correct stacktrace.
    tailcall_trace_buf: Vec<(NixString, Option<SourcePosition>)>,
}

impl<'gc> Evaluator<'gc> {
    pub fn new(gc_handle: &'gc mut GcHandle) -> Result<Self, GcError> {
        let builtins = get_builtins(gc_handle)?;
        Ok(Self {
            gc_handle,
            stack_cache: Vec::new(),
            thunk_alloc_buffer: Vec::new(),
            builtins: Rc::new(builtins),
            error_message_printed: false,
        })
    }
    pub fn eval_expression(&mut self, thunk: Thunk) -> Result<NixValue, EvaluateError> {
        let ptr = self.gc_handle.alloc(thunk)?;
        self.force_thunk(ptr)
    }

    pub fn force_thunk(&mut self, mut thunk: GcPointer<Thunk>) -> Result<NixValue, EvaluateError> {
        match self.gc_handle.load(&thunk) {
            Thunk::Blackhole => return Err(EvaluateError::BlackholeEvaluated),
            Thunk::Value(v) => return Ok(v.clone()),
            Thunk::Deferred { context, code } => {
                let mut state = self.stack_cache.pop().unwrap_or_default();
                state
                    .context
                    .extend_from_slice(self.gc_handle.load(&context.entries).as_ref());
                state
                    .code_buf
                    .extend_from_slice(self.gc_handle.load(code).as_ref());
                state.code_source_positions = context.source_positions.clone();
                let source_filename = context.source_filename.clone();

                let blackhole = self.gc_handle.alloc(Thunk::Blackhole)?;
                thunk = self.gc_handle.replace(&thunk, blackhole);

                let result = ThunkEvaluator {
                    state,
                    evaluator: self,
                    source_filename: source_filename.clone(),
                    insn_counter: 0,
                    tailcall_trace_buf: Vec::new(),
                }
                .compute_result()?;

                let result_ptr = self.gc_handle.alloc(Thunk::Value(result.clone()))?;
                self.gc_handle.replace(&thunk, result_ptr);

                Ok(result)
            }
        }
    }

    pub fn evaluate_call(
        &mut self,
        func: value::Function,
        arg: GcPointer<Thunk>,
    ) -> Result<NixValue, EvaluateError> {
        let mut sub_state = self.stack_cache.pop().unwrap_or_default();
        let source_filename = func.context.source_filename.clone();
        sub_state.initialize_for_call(func, arg, self)?;

        ThunkEvaluator {
            state: sub_state,
            evaluator: self,
            source_filename,
            insn_counter: 0,
            tailcall_trace_buf: Vec::new(),
        }
        .compute_result()
    }
}

impl<'eval, 'gc> ThunkEvaluator<'eval, 'gc> {
    fn compute_result(mut self) -> Result<NixValue, EvaluateError> {
        let res = self.compute_result_inner();

        // print the stack trace....
        if let Err(e) = &res {
            if !self.evaluator.error_message_printed {
                println!("\n\nError: {e:?}\n\n");
                self.evaluator.error_message_printed = true;
            }

            let source_filename = self.source_filename.load(&self.evaluator.gc_handle);
            if let Some(source_pos) =
                self.state.code_source_positions.as_ref().map(|ptr| {
                    self.evaluator.gc_handle.load(ptr).as_ref()[self.insn_counter].clone()
                })
            {
                println!("at {source_filename} {source_pos}");
            } else {
                println!("at {source_filename}",);
            }

            // print the tailcall entries if present
            for (filename, pos) in self.tailcall_trace_buf.iter().rev() {
                if let Some(pos) = pos {
                    println!(
                        "at {} {} (tailcall)",
                        filename.load(&self.evaluator.gc_handle),
                        pos
                    );
                } else {
                    println!("at {} (tailcall)", filename.load(&self.evaluator.gc_handle),);
                }
            }
        }

        res
    }
    fn compute_result_inner(&mut self) -> Result<NixValue, EvaluateError> {
        'outer: loop {
            #[cfg(test)]
            println!(
                "forcing thunk at {}, {:?} ....",
                self.source_filename.load(&self.evaluator.gc_handle),
                self.state.code_source_positions.as_ref().map(|p| self
                    .evaluator
                    .gc_handle
                    .load(p)
                    .as_ref()[0]
                    .clone())
            );

            let mut code_buf = core::mem::take(&mut self.state.code_buf);
            {
                let mut code = code_buf.drain(..);

                while let Some(opcode) = code.next() {
                    #[cfg(test)]
                    println!("executing: {:?}", opcode.debug(&self.evaluator.gc_handle));

                    match opcode {
                        VmOp::AllocList(list_len) => {
                            let thunk_buf = &mut self.evaluator.thunk_alloc_buffer;
                            let end = self.state.thunk_stack.len();
                            let start = end - list_len as usize;
                            thunk_buf.extend(self.state.thunk_stack.drain(start..end));
                            let entries = self.evaluator.gc_handle.alloc_vec(thunk_buf)?;
                            self.state
                                .local_stack
                                .push(NixValue::List(value::List { entries }));
                        }

                        VmOp::AllocLambda(args) => {
                            let LambdaAllocArgs {
                                code,
                                context_build_instructions,
                                call_requirements,
                                source_file,
                                source_locations,
                            } = self.evaluator.gc_handle.load(&args);

                            let code = code.clone();
                            let call_requirements = call_requirements.clone();
                            let source_locations = source_locations.clone();

                            let alloc_buf = &mut self.evaluator.thunk_alloc_buffer;
                            fetch_context_entries(
                                alloc_buf,
                                &self.state,
                                self.evaluator
                                    .gc_handle
                                    .load(context_build_instructions)
                                    .as_ref(),
                            );
                            let context = ExecutionContext {
                                source_positions: Some(source_locations),
                                source_filename: source_file.clone(),
                                entries: self.evaluator.gc_handle.alloc_vec(alloc_buf)?,
                            };

                            self.state.local_stack.push(NixValue::Function(Function {
                                context,
                                code,
                                call_type: call_requirements,
                            }));
                        }

                        VmOp::BuildAttrset(num_keys) => {
                            let attrset = if num_keys > 0 {
                                let mut entries = Vec::new();
                                for _ in 0..num_keys {
                                    let key = match self.pop()? {
                                        NixValue::String(key) => key,
                                        _ => return Err(EvaluateError::TypeError),
                                    };
                                    let value = self
                                        .state
                                        .thunk_stack
                                        .pop()
                                        .expect("thunk stack exhausted unexpectedly");
                                    entries.push((key, value));
                                }
                                Attrset::build_from_entries(
                                    &mut entries,
                                    &mut self.evaluator.gc_handle,
                                )?
                            } else {
                                let entries = self.evaluator.gc_handle.alloc_slice(&[])?;
                                value::Attrset { entries }
                            };

                            self.state.local_stack.push(NixValue::Attrset(attrset));
                        }

                        VmOp::LoadThunk(ValueSource::ContextReference(idx)) => {
                            let thunk = self.state.context[idx as usize].clone();
                            let value = self.evaluator.force_thunk(thunk)?;
                            self.state.local_stack.push(value);
                        }
                        VmOp::LoadThunk(ValueSource::ThunkStackRef(idx)) => {
                            let thunk = self.state.thunk_stack[idx as usize].clone();
                            let value = self.evaluator.force_thunk(thunk)?;
                            self.state.local_stack.push(value);
                        }
                        VmOp::PushBlackholes(count) => {
                            for _ in 0..count {
                                let blackhole = self.evaluator.gc_handle.alloc(Thunk::Blackhole)?;
                                self.state.thunk_stack.push(blackhole);
                            }
                        }
                        VmOp::DropThunks(count) => {
                            let thunkstack = &mut self.state.thunk_stack;
                            let new_len = thunkstack.len() - count as usize;
                            thunkstack.truncate(new_len);
                        }

                        VmOp::AllocateThunk(args) => {
                            self.execute_alloc_thunk(args)?;
                        }

                        VmOp::DuplicateThunk(source) => {
                            let thunk = match source {
                                crate::vm::opcodes::ValueSource::ContextReference(idx) => {
                                    self.state.context[idx as usize].clone()
                                }
                                crate::vm::opcodes::ValueSource::ThunkStackRef(idx) => {
                                    self.state.thunk_stack[idx as usize].clone()
                                }
                            };

                            self.state.thunk_stack.push(thunk);
                        }

                        VmOp::Skip(to_skip) => {
                            (&mut code).take(to_skip as usize).for_each(|_| {});
                        }
                        VmOp::SkipUnless(to_skip) => {
                            if let NixValue::Bool(execute_next_insn) = self.pop()? {
                                if !execute_next_insn {
                                    (&mut code).take(to_skip as usize).for_each(|_| {});
                                }
                            } else {
                                return Err(EvaluateError::TypeError);
                            }
                        }
                        VmOp::ConcatLists(count) => self.execute_concat_lists(count)?,
                        VmOp::Add => {
                            let right = self.pop()?;
                            let left = self.pop()?;

                            if let NixValue::String(left) = left {
                                let right = right.expect_string()?;
                                let result = left.concat(right, self.evaluator.gc_handle)?;
                                self.state.local_stack.push(NixValue::String(result));
                            } else {
                                self.state.local_stack.push(execute_arithmetic_op(
                                    left,
                                    right,
                                    |l, r| l + r,
                                    |l, r| l + r,
                                )?);
                            }
                        }
                        VmOp::Mul => {
                            let right = self.pop()?;
                            let left = self.pop()?;
                            self.state.local_stack.push(execute_arithmetic_op(
                                left,
                                right,
                                |l, r| l * r,
                                |l, r| l * r,
                            )?);
                        }
                        VmOp::Div => {
                            let right = self.pop()?;
                            let left = self.pop()?;
                            self.state.local_stack.push(execute_arithmetic_op(
                                left,
                                right,
                                |l, r| l / r,
                                |l, r| l / r,
                            )?);
                        }
                        VmOp::NumericNegate => {
                            let result = match self.pop()? {
                                NixValue::Int(i) => NixValue::Int(-i),
                                NixValue::Float(f) => NixValue::Float(-f),
                                _ => return Err(EvaluateError::TypeError),
                            };
                            self.state.local_stack.push(result);
                        }
                        VmOp::BinaryNot => {
                            let result = match self.pop()? {
                                NixValue::Bool(b) => NixValue::Bool(!b),
                                _ => return Err(EvaluateError::TypeError),
                            };
                            self.state.local_stack.push(result);
                        }

                        VmOp::Call => {
                            let arg = self
                                .state
                                .thunk_stack
                                .pop()
                                .expect("thunk stack exhausted unexpectedly");
                            let result = match self.pop()? {
                                NixValue::Function(function) => {
                                    self.evaluator.evaluate_call(function, arg)
                                }
                                NixValue::Builtin(builtin) => self
                                    .evaluator
                                    .builtins
                                    .clone()
                                    .execute_builtin(builtin, arg, &mut self.evaluator),
                                _ => Err(EvaluateError::TypeError),
                            }?;

                            self.state.local_stack.push(result);
                        }
                        VmOp::TailCall => {
                            // Tail call! first save the argument, then clear all state
                            let arg = self
                                .state
                                .thunk_stack
                                .pop()
                                .expect("thunk stack exhausted unexpectedly");
                            let func = self.pop()?;

                            // put the code buffer back to avoid a reallocation
                            core::mem::drop(code);
                            self.state.code_buf = code_buf;

                            match func {
                                NixValue::Builtin(builtin) => {
                                    // a builtin does not really need to use our stack frame.
                                    // Instead just return straight from here.
                                    return self.evaluator.builtins.clone().execute_builtin(
                                        builtin,
                                        arg,
                                        &mut self.evaluator,
                                    );
                                }
                                NixValue::Function(func) => {
                                    // grab the current location for future reuse.
                                    let current_source_pos =
                                        self.state.code_source_positions.as_ref().map(|ptr| {
                                            self.evaluator.gc_handle.load(&ptr).as_ref()
                                                [self.insn_counter]
                                                .clone()
                                        });
                                    let current_filename = core::mem::replace(
                                        &mut self.source_filename,
                                        func.context.source_filename.clone(),
                                    );

                                    // reinitialize our context and continue from the top.
                                    self.insn_counter = 0;
                                    self.state.clear();
                                    self.state.initialize_for_call(func, arg, self.evaluator)?;

                                    self.tailcall_trace_buf
                                        .push((current_filename, current_source_pos));

                                    continue 'outer;
                                }
                                _ => return Err(EvaluateError::TypeError),
                            }
                        }

                        VmOp::CastToPath { source_location } => {
                            let path_value = self.pop()?.expect_string()?;
                            let path = PathValue::new(
                                source_location,
                                path_value,
                                &mut self.evaluator.gc_handle,
                            )?;

                            self.state.local_stack.push(NixValue::Path(path));
                        }
                        VmOp::PushImmediate(imm) => {
                            let imm = self.evaluator.gc_handle.load(&imm);
                            self.state.local_stack.push(imm.clone());
                        }
                        VmOp::Compare(compare_mode) => {
                            let right = self.pop()?;
                            let left = self.pop()?;

                            let result = match (
                                compare_mode,
                                compare_values(&mut self.evaluator, &left, &right)?,
                            ) {
                                (crate::vm::opcodes::CompareMode::Equal, Some(Ordering::Equal)) => {
                                    true
                                }
                                (crate::vm::opcodes::CompareMode::Equal, _) => false,
                                (
                                    crate::vm::opcodes::CompareMode::NotEqual,
                                    Some(Ordering::Equal),
                                ) => false,
                                (crate::vm::opcodes::CompareMode::NotEqual, _) => true,
                                (
                                    crate::vm::opcodes::CompareMode::LessThanStrict,
                                    Some(Ordering::Less),
                                ) => true,
                                (crate::vm::opcodes::CompareMode::LessThanStrict, _) => false,
                                (
                                    crate::vm::opcodes::CompareMode::LessThanOrEqual,
                                    Some(Ordering::Less | Ordering::Equal),
                                ) => true,
                                (crate::vm::opcodes::CompareMode::LessThanOrEqual, _) => false,
                                (
                                    crate::vm::opcodes::CompareMode::GreaterThanStrict,
                                    Some(Ordering::Greater),
                                ) => true,
                                (crate::vm::opcodes::CompareMode::GreaterThanStrict, _) => false,
                                (
                                    crate::vm::opcodes::CompareMode::GreaterOrEqual,
                                    Some(Ordering::Greater | Ordering::Equal),
                                ) => true,
                                (crate::vm::opcodes::CompareMode::GreaterOrEqual, _) => false,
                            };

                            self.state.local_stack.push(NixValue::Bool(result));
                        }
                        VmOp::Sub => {
                            let right = self.pop()?;
                            let left = self.pop()?;
                            self.state.local_stack.push(execute_arithmetic_op(
                                left,
                                right,
                                |l, r| l - r,
                                |l, r| l - r,
                            )?);
                        }
                        VmOp::MergeAttrsets => {
                            let added_set = self.pop()?.expect_attrset()?;
                            let base_set = self.pop()?.expect_attrset()?;

                            self.state.local_stack.push(NixValue::Attrset(
                                base_set.merge(added_set, self.evaluator.gc_handle)?,
                            ));
                        }
                        VmOp::GetAttribute { push_error } => {
                            let attrset = self.pop()?.expect_attrset()?;
                            let key = self.pop()?.expect_string()?;

                            let value = attrset.get_entry(&self.evaluator.gc_handle, &key);

                            if push_error {
                                if let Some(val) = value {
                                    self.state
                                        .local_stack
                                        .push(self.evaluator.force_thunk(val)?);
                                    self.state.local_stack.push(NixValue::Bool(false));
                                } else {
                                    self.state.local_stack.push(NixValue::Bool(true));
                                }
                            } else {
                                let val =
                                    value.ok_or_else(|| EvaluateError::AttrsetKeyNotFound {
                                        attr_name: key.load(&self.evaluator.gc_handle).to_owned(),
                                    })?;
                                self.state
                                    .local_stack
                                    .push(self.evaluator.force_thunk(val)?);
                            }
                        }
                        VmOp::HasAttribute => {
                            let result = match (self.pop()?, self.pop()?) {
                                (NixValue::Attrset(attrset), NixValue::String(key)) => {
                                    let attrset_slice =
                                        self.evaluator.gc_handle.load(&attrset.entries).as_ref();
                                    let key_str = key.load(&self.evaluator.gc_handle);

                                    attrset_slice
                                        .binary_search_by_key(&key_str, |(k, _)| {
                                            k.load(&self.evaluator.gc_handle)
                                        })
                                        .is_ok()
                                }
                                _ => false,
                            };

                            self.state.local_stack.push(NixValue::Bool(result));
                        }
                        VmOp::ConcatStrings(num) => {
                            self.execute_concat_strings(num)?;
                        }
                        VmOp::OverwriteThunk { stackref } => {
                            let mut new_thunk =
                                self.state.thunk_stack.pop().expect("a thunk to be present");
                            let target_thunk = &mut self.state.thunk_stack[stackref as usize];
                            new_thunk = self.evaluator.gc_handle.replace(&target_thunk, new_thunk);
                            *target_thunk = new_thunk;
                        }
                    }
                    self.insn_counter += 1;
                }
            }

            self.state.code_buf = code_buf;
            let result_value = self.pop()?;
            #[cfg(test)]
            println!(
                "thunk evaluated to {:?}",
                result_value.debug(&self.evaluator.gc_handle)
            );
            return Ok(result_value);
        }
    }

    fn execute_alloc_thunk(
        &mut self,
        args: GcPointer<crate::vm::opcodes::ThunkAllocArgs>,
    ) -> Result<(), EvaluateError> {
        let args = self.evaluator.gc_handle.load(&args);
        let code = args.code.clone();
        let build_instructions = self
            .evaluator
            .gc_handle
            .load(&args.context_build_instructions)
            .as_ref();

        #[cfg(test)]
        println!("ctx: {build_instructions:?}");

        let thunk_buf = &mut self.evaluator.thunk_alloc_buffer;
        fetch_context_entries(thunk_buf, &self.state, build_instructions);
        let context = ExecutionContext {
            source_positions: Some(args.source_positions.clone()),
            entries: self.evaluator.gc_handle.alloc_vec(thunk_buf)?,
            source_filename: self.source_filename.clone(),
        };

        let new_thunk = self
            .evaluator
            .gc_handle
            .alloc(Thunk::Deferred { context, code })?;

        self.state.thunk_stack.push(new_thunk);
        Ok(())
    }

    fn pop(&mut self) -> Result<NixValue, EvaluateError> {
        self.state
            .local_stack
            .pop()
            .ok_or(EvaluateError::ExecutionStackExhaustedUnexpectedly)
    }

    fn execute_concat_strings(&mut self, count: u32) -> Result<(), EvaluateError> {
        let source = core::iter::repeat_with(|| {
            self.state
                .local_stack
                .pop()
                .ok_or(EvaluateError::ExecutionStackExhaustedUnexpectedly)?
                .expect_string()
        })
        .take(count as usize);
        let result = NixString::concat_many(source, &mut self.evaluator.gc_handle)?;

        self.state.local_stack.push(NixValue::String(result));

        Ok(())
    }

    fn execute_concat_lists(&mut self, count: u32) -> Result<(), EvaluateError> {
        let result = if count == 2 {
            let left = self.pop()?.expect_list()?;
            let right = self.pop()?.expect_list()?;
            List::concat_lists(&[left, right], &mut self.evaluator.gc_handle)?
        } else {
            let lists = core::iter::repeat_with(|| self.pop()?.expect_list())
                .take(count as usize)
                .collect::<Result<Vec<_>, _>>()?;
            List::concat_lists(&lists, &mut self.evaluator.gc_handle)?
        };

        self.state.local_stack.push(NixValue::List(result));
        Ok(())
    }
}

impl Drop for ThunkEvaluator<'_, '_> {
    fn drop(&mut self) {
        self.state.clear();
        self.evaluator
            .stack_cache
            .push(core::mem::take(&mut self.state));
    }
}

fn fetch_context_entries(
    dest: &mut Vec<GcPointer<Thunk>>,
    state: &ThunkEvalState,
    instructions: &[crate::vm::opcodes::ValueSource],
) {
    dest.clear();
    for insn in instructions {
        match insn {
            crate::vm::opcodes::ValueSource::ContextReference(ctxref) => {
                dest.push(state.context[(*ctxref) as usize].clone())
            }
            crate::vm::opcodes::ValueSource::ThunkStackRef(stackref) => {
                dest.push(state.thunk_stack[(*stackref) as usize].clone())
            }
        }
    }
}

fn execute_arithmetic_op(
    l: NixValue,
    r: NixValue,
    int_op: impl FnOnce(i64, i64) -> i64,
    float_op: impl FnOnce(f64, f64) -> f64,
) -> Result<NixValue, EvaluateError> {
    match (l, r) {
        (NixValue::Int(l), NixValue::Int(r)) => Ok(NixValue::Int(int_op(l, r))),
        (NixValue::Int(l), NixValue::Float(r)) => Ok(NixValue::Float(float_op(l as f64, r))),
        (NixValue::Float(l), NixValue::Int(r)) => Ok(NixValue::Float(float_op(l, r as f64))),
        (NixValue::Float(l), NixValue::Float(r)) => Ok(NixValue::Float(float_op(l, r))),
        _ => Err(EvaluateError::TypeError),
    }
}

fn compare_values(
    evaluator: &mut Evaluator,
    l: &NixValue,
    r: &NixValue,
) -> Result<Option<Ordering>, EvaluateError> {
    let gc_handle = &evaluator.gc_handle;
    let res = match (l, r) {
        (NixValue::String(l), NixValue::String(r)) => {
            let l = l.load(gc_handle);
            let r = r.load(gc_handle);
            Some(l.cmp(r))
        }
        (NixValue::Bool(l), NixValue::Bool(r)) => Some(l.cmp(&r)),
        (NixValue::Null, NixValue::Null) => Some(Ordering::Equal),
        (NixValue::Int(l), NixValue::Int(r)) => Some(l.cmp(&r)),
        (NixValue::Int(l), NixValue::Float(r)) => (*l as f64).partial_cmp(&r),
        (NixValue::Float(l), NixValue::Int(r)) => l.partial_cmp(&(*r as f64)),
        (NixValue::Float(l), NixValue::Float(r)) => l.partial_cmp(&r),
        (NixValue::Path(l), NixValue::Path(r)) => {
            let l = l.resolved.load(gc_handle);
            let r = r.resolved.load(gc_handle);
            Some(l.cmp(r))
        }
        (NixValue::Attrset(l), NixValue::Attrset(r)) => return compare_attrsets(evaluator, l, r),
        (NixValue::List(l), NixValue::List(r)) => return compare_lists(evaluator, l, r),
        _ => None,
    };

    Ok(res)
}

fn compare_attrsets(
    evaluator: &mut Evaluator,
    left: &Attrset,
    right: &Attrset,
) -> Result<Option<Ordering>, EvaluateError> {
    // first try to detect a difference by looking at just the keys.
    let number_of_keys = {
        let left = evaluator.gc_handle.load(&left.entries).as_ref();
        let right = evaluator.gc_handle.load(&right.entries).as_ref();

        if left.len() != right.len() {
            return Ok(None);
        }
        if left.is_empty() {
            return Ok(Some(Ordering::Equal));
        }

        let keys_are_equal = left
            .iter()
            .map(|e| e.0.load(&evaluator.gc_handle))
            .eq(right.iter().map(|e| e.0.load(&evaluator.gc_handle)));

        if !keys_are_equal {
            return Ok(None);
        }

        left.len()
    };

    // if we got to this point, the attrests are identical from the key perspective (number and
    // count).
    // Now we just need to compare the values.
    for key_idx in 0..number_of_keys {
        let left = evaluator.gc_handle.load(&left.entries).as_ref()[key_idx]
            .1
            .clone();
        let right = evaluator.gc_handle.load(&right.entries).as_ref()[key_idx]
            .1
            .clone();
        let left_value = evaluator.force_thunk(left)?;
        let right_value = evaluator.force_thunk(right)?;

        if compare_values(evaluator, &left_value, &right_value)? != Some(Ordering::Equal) {
            return Ok(None);
        }
    }

    // we could not find a difference
    Ok(Some(Ordering::Equal))
}

fn compare_lists(
    evaluator: &mut Evaluator,
    left: &List,
    right: &List,
) -> Result<Option<Ordering>, EvaluateError> {
    let mut compared_to_idx: usize = 0;
    loop {
        let (l_thunk, r_thunk) = {
            let gc_handle = &evaluator.gc_handle;
            let slice_l = &gc_handle.load(&left.entries).as_ref()[compared_to_idx..];
            let slice_r = &gc_handle.load(&right.entries).as_ref()[compared_to_idx..];
            match (slice_l.first(), slice_r.first()) {
                (None, None) => return Ok(Some(Ordering::Equal)),
                (None, Some(_)) => return Ok(Some(Ordering::Less)),
                (Some(_), None) => return Ok(Some(Ordering::Greater)),
                (Some(l), Some(r)) => (l.clone(), r.clone()),
            }
        };

        let l_value = evaluator.force_thunk(l_thunk)?;
        let r_value = evaluator.force_thunk(r_thunk)?;

        match compare_values(evaluator, &l_value, &r_value)? {
            Some(Ordering::Equal) => {
                // we compared the value completely and need to continue with the next one.
                compared_to_idx += 1;
            }
            res => return Ok(res),
        }
    }
}

impl NixValue {
    pub fn as_attrset(&self) -> Result<&Attrset, EvaluateError> {
        match self {
            NixValue::Attrset(a) => Ok(a),
            _ => Err(EvaluateError::TypeError),
        }
    }
    pub fn expect_attrset(self) -> Result<Attrset, EvaluateError> {
        match self {
            NixValue::Attrset(a) => Ok(a),
            _ => Err(EvaluateError::TypeError),
        }
    }

    pub fn expect_string(self) -> Result<value::NixString, EvaluateError> {
        match self {
            NixValue::String(s) => Ok(s),
            _ => Err(EvaluateError::TypeError),
        }
    }

    pub fn expect_list(self) -> Result<value::List, EvaluateError> {
        match self {
            NixValue::List(l) => Ok(l),
            _ => Err(EvaluateError::TypeError),
        }
    }

    pub fn expect_int(self) -> Result<i64, EvaluateError> {
        match self {
            NixValue::Int(i) => Ok(i),
            _ => Err(EvaluateError::TypeError),
        }
    }
}

impl ThunkEvalState {
    pub fn clear(&mut self) {
        self.context.clear();
        self.local_stack.clear();
        self.thunk_stack.clear();
        self.code_buf.clear();
        self.code_source_positions = None;
    }

    pub fn initialize_for_call(
        &mut self,
        func: value::Function,
        arg: GcPointer<Thunk>,
        evaluator: &mut Evaluator,
    ) -> Result<(), EvaluateError> {
        // check that all required keys are present if needed
        match func.call_type {
            crate::vm::opcodes::LambdaCallType::Simple => {
                // nothing to validate here.
            }
            crate::vm::opcodes::LambdaCallType::Attrset {
                keys,
                includes_rest_pattern,
            } => {
                let arg = evaluator.force_thunk(arg.clone())?;
                let arg = arg.as_attrset()?;
                let keys = evaluator.gc_handle.load(&keys).as_ref();

                // check that no unexpected args were provided
                if !includes_rest_pattern {
                    for key in arg.keys(&evaluator.gc_handle) {
                        if !keys
                            .iter()
                            .any(|(nix_str, _)| nix_str.load(&evaluator.gc_handle) == key)
                        {
                            return Err(EvaluateError::CallWithUnexpectedArg {
                                arg_name: key.to_owned(),
                            });
                        }
                    }
                }

                // and check that all required args are here.
                for expected_arg in
                    keys.iter().filter_map(
                        |(arg_name, is_required)| {
                            if *is_required {
                                Some(arg_name)
                            } else {
                                None
                            }
                        },
                    )
                {
                    if arg.get_entry(&evaluator.gc_handle, expected_arg).is_none() {
                        let arg_name = expected_arg.load(&evaluator.gc_handle).to_owned();
                        return Err(EvaluateError::CallWithMissingArg { arg_name });
                    }
                }
            }
        }

        self.thunk_stack.push(arg);
        self.code_buf
            .extend_from_slice(evaluator.gc_handle.load(&func.code).as_ref());
        self.context
            .extend_from_slice(evaluator.gc_handle.load(&func.context.entries).as_ref());
        self.code_source_positions = func.context.source_positions;

        Ok(())
    }
}
