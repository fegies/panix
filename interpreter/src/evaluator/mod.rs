use std::cmp::Ordering;

use gc::{specialized_types::array::Array, GcHandle, GcPointer};

use crate::{
    compiler::ValueSource,
    vm::{
        opcodes::{ExecutionContext, LambdaAllocArgs, VmOp},
        value::{self, Attrset, Function, List, NixValue, Thunk},
    },
    EvaluateError,
};

use super::InterpreterError;

pub struct Evaluator<'gc> {
    gc_handle: &'gc mut GcHandle,
    stack_cache: Vec<ThunkEvalState>,
    thunk_alloc_buffer: Vec<GcPointer<Thunk>>,
}
#[derive(Default, Debug)]
struct ThunkEvalState {
    context: Vec<GcPointer<Thunk>>,
    local_stack: Vec<NixValue>,
    thunk_stack: Vec<GcPointer<Thunk>>,
    code_buf: Vec<VmOp>,
}

struct ThunkEvaluator<'eval, 'gc> {
    state: ThunkEvalState,
    evaluator: &'eval mut Evaluator<'gc>,
}

impl<'gc> Evaluator<'gc> {
    pub fn new(gc_handle: &'gc mut GcHandle) -> Self {
        Self {
            gc_handle,
            stack_cache: Vec::new(),
            thunk_alloc_buffer: Vec::new(),
        }
    }
    pub fn eval_expression(&mut self, mut thunk: Thunk) -> Result<NixValue, EvaluateError> {
        let ptr = self.gc_handle.alloc(thunk)?;
        self.force_thunk(ptr)
    }

    fn force_thunk(&mut self, thunk: GcPointer<Thunk>) -> Result<NixValue, EvaluateError> {
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

                let blackhole = self.gc_handle.alloc(Thunk::Blackhole)?;
                self.gc_handle.replace(&thunk, blackhole);

                let result = ThunkEvaluator {
                    state,
                    evaluator: self,
                }
                .compute_result()?;

                let result_ptr = self.gc_handle.alloc(Thunk::Value(result.clone()))?;
                self.gc_handle.replace(&thunk, result_ptr);

                Ok(result)
            }
        }
    }
}

impl<'eval, 'gc> ThunkEvaluator<'eval, 'gc> {
    fn compute_result(mut self) -> Result<NixValue, EvaluateError> {
        let mut code_buf = core::mem::take(&mut self.state.code_buf);

        println!("forcing thunk...");
        {
            let mut code = code_buf.drain(..);

            while let Some(opcode) = code.next() {
                println!("executing: {opcode:?}");
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
                        } = self.evaluator.gc_handle.load(&args);

                        let code = code.clone();
                        let call_requirements = call_requirements.clone();

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
                            entries: self.evaluator.gc_handle.alloc_vec(alloc_buf)?,
                        };

                        self.state.local_stack.push(NixValue::Function(Function {
                            context,
                            code,
                            call_type: call_requirements,
                        }));
                    }

                    VmOp::BuildAttrset(num_keys) => {
                        let entries = if num_keys > 0 {
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
                            entries.sort_by(|(a, _), (b, _)| {
                                a.load(&self.evaluator.gc_handle)
                                    .cmp(b.load(&self.evaluator.gc_handle))
                            });

                            entries.dedup_by(|(a, _), (b, _)| {
                                a.load(&self.evaluator.gc_handle)
                                    == b.load(&self.evaluator.gc_handle)
                            });

                            if entries.len() < num_keys as usize {
                                return Err(EvaluateError::DuplicateAttrsetKey);
                            }

                            self.evaluator.gc_handle.alloc_vec(&mut entries)?
                        } else {
                            self.evaluator.gc_handle.alloc_slice(&[])?
                        };

                        self.state
                            .local_stack
                            .push(NixValue::Attrset(value::Attrset { entries }));
                    }

                    VmOp::LoadContext(idx) => {
                        let thunk = self.state.context[idx.0 as usize].clone();
                        let value = self.evaluator.force_thunk(thunk)?;
                        self.state.local_stack.push(value);
                    }
                    VmOp::LoadLocalThunk(idx) => {
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

                    VmOp::AllocateThunk { slot, args } => {
                        let args = self.evaluator.gc_handle.load(&args);
                        let code = args.code.clone();
                        let build_instructions = self
                            .evaluator
                            .gc_handle
                            .load(&args.context_build_instructions)
                            .as_ref();
                        println!("ctx: {build_instructions:?}");

                        let thunk_buf = &mut self.evaluator.thunk_alloc_buffer;
                        fetch_context_entries(thunk_buf, &self.state, build_instructions);
                        let context = ExecutionContext {
                            entries: self.evaluator.gc_handle.alloc_vec(thunk_buf)?,
                        };

                        let new_thunk = self
                            .evaluator
                            .gc_handle
                            .alloc(Thunk::Deferred { context, code })?;
                        if let Some(slot) = slot {
                            let dest_idx = self.state.thunk_stack.len() - 1 - slot as usize;
                            self.state.thunk_stack[dest_idx] = new_thunk;
                        } else {
                            self.state.thunk_stack.push(new_thunk);
                        }
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
                    VmOp::ConcatLists(_) => todo!(),
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
                        let arg = self.pop()?;
                        let func = if let NixValue::Function(func) = self.pop()? {
                            func
                        } else {
                            return Err(EvaluateError::TypeError);
                        };

                        // check that all required keys are present if needed
                        match func.call_type {
                            crate::vm::opcodes::LambdaCallType::Simple => {
                                // nothing to validate here.
                            }
                            crate::vm::opcodes::LambdaCallType::Attrset {
                                keys,
                                includes_rest_pattern,
                            } => {
                                let arg = arg.as_attrset()?;
                                let keys = self.evaluator.gc_handle.load(&keys).as_ref();

                                // check that no unexpected args were provided
                                if !includes_rest_pattern {
                                    for key in arg.keys(&self.evaluator.gc_handle) {
                                        if !keys.iter().any(|(nix_str, _)| {
                                            nix_str.load(&self.evaluator.gc_handle) == key
                                        }) {
                                            return Err(EvaluateError::CallWithUnexpectedArg {
                                                arg_name: key.to_owned(),
                                            });
                                        }
                                    }
                                }

                                // and check that all required args are here.
                                for expected_arg in
                                    keys.iter().filter_map(|(arg_name, is_required)| {
                                        if *is_required {
                                            Some(arg_name)
                                        } else {
                                            None
                                        }
                                    })
                                {
                                    if arg
                                        .get_entry(&self.evaluator.gc_handle, expected_arg)
                                        .is_none()
                                    {
                                        let arg_name =
                                            expected_arg.load(&self.evaluator.gc_handle).to_owned();
                                        return Err(EvaluateError::CallWithMissingArg { arg_name });
                                    }
                                }
                            }
                        }

                        let mut sub_state = self.evaluator.stack_cache.pop().unwrap_or_default();
                        sub_state
                            .thunk_stack
                            .push(self.evaluator.gc_handle.alloc(Thunk::Value(arg))?);
                        sub_state
                            .code_buf
                            .extend_from_slice(self.evaluator.gc_handle.load(&func.code).as_ref());
                        sub_state.context.extend_from_slice(
                            self.evaluator
                                .gc_handle
                                .load(&func.context.entries)
                                .as_ref(),
                        );

                        let result = ThunkEvaluator {
                            state: sub_state,
                            evaluator: self.evaluator,
                        }
                        .compute_result()?;

                        self.state.local_stack.push(result);
                    }
                    VmOp::CastToPath => {
                        let value = if let NixValue::String(s) = self.pop()? {
                            s
                        } else {
                            return Err(EvaluateError::TypeError);
                        };

                        self.state.local_stack.push(NixValue::Path(value));
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
                            (crate::vm::opcodes::CompareMode::Equal, Some(Ordering::Equal)) => true,
                            (crate::vm::opcodes::CompareMode::Equal, _) => false,
                            (crate::vm::opcodes::CompareMode::NotEqual, Some(Ordering::Equal)) => {
                                false
                            }
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
                    VmOp::MergeAttrsets => todo!(),
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
                            let val = value.ok_or_else(|| EvaluateError::AttrsetKeyNotFound)?;
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
                    VmOp::PushBuiltin(builtin) => todo!(),
                    VmOp::ConcatStrings(num) => {
                        let mut result = self.pop()?.expect_string()?;
                        for _ in 1..num {
                            let suffix = self.pop()?.expect_string()?;
                            result = result.concat(suffix, &mut self.evaluator.gc_handle)?;
                        }

                        self.state.local_stack.push(NixValue::String(result));
                    }
                }
            }
        }

        self.state.code_buf = code_buf;
        let result_value = self.pop()?;

        println!("thunk evaluated to {result_value:?}");

        Ok(result_value)
    }

    fn pop(&mut self) -> Result<NixValue, EvaluateError> {
        self.state
            .local_stack
            .pop()
            .ok_or(EvaluateError::ExecutionStackExhaustedUnexpectedly)
    }
}

impl Drop for ThunkEvaluator<'_, '_> {
    fn drop(&mut self) {
        self.state.context.clear();
        self.state.local_stack.clear();
        self.state.thunk_stack.clear();
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

fn bool_op(
    l: &NixValue,
    r: &NixValue,
    op: impl FnOnce(bool, bool) -> bool,
) -> Result<NixValue, EvaluateError> {
    match (l, r) {
        (NixValue::Bool(l), NixValue::Bool(r)) => Ok(NixValue::Bool(op(*l, *r))),
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
            let l = l.load(gc_handle);
            let r = r.load(gc_handle);
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
        if (left.is_empty()) {
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
}
