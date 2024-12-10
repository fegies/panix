use std::cmp::Ordering;

use gc::{GcHandle, GcPointer};

use crate::{
    vm::{
        opcodes::{ExecutionContext, VmOp},
        value::{NixValue, Thunk},
    },
    EvaluateError,
};

use super::InterpreterError;

pub struct Evaluator<'gc> {
    gc_handle: &'gc mut GcHandle,
    stack_cache: Vec<ThunkEvalState>,
    thunk_alloc_buffer: Vec<GcPointer<Thunk>>,
}
#[derive(Default)]
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
        self.get_evaluator().force_thunk(ptr)
    }
    fn get_evaluator(&mut self) -> ThunkEvaluator<'_, 'gc> {
        let state = self.stack_cache.pop().unwrap_or_default();
        ThunkEvaluator {
            state,
            evaluator: self,
        }
    }
}

impl<'eval, 'gc> ThunkEvaluator<'eval, 'gc> {
    fn force_thunk(&mut self, thunk: GcPointer<Thunk>) -> Result<NixValue, EvaluateError> {
        match self.evaluator.gc_handle.load(&thunk) {
            Thunk::Blackhole => return Err(EvaluateError::BlackholeEvaluated),
            Thunk::Value(v) => return Ok(v.clone()),
            Thunk::Deferred { context, code } => {
                self.state.context.clear();
                self.state
                    .context
                    .extend_from_slice(self.evaluator.gc_handle.load(&context.entries).as_ref());
                self.state.code_buf.clear();
                self.state
                    .code_buf
                    .extend_from_slice(self.evaluator.gc_handle.load(code).as_ref());
            }
        }

        let blackhole = self.evaluator.gc_handle.alloc(Thunk::Blackhole)?;
        self.evaluator.gc_handle.replace(&thunk, blackhole);
        let mut code_buf = core::mem::take(&mut self.state.code_buf);

        println!("forcing thunk...");
        {
            let mut code = code_buf.drain(..);

            while let Some(opcode) = code.next() {
                println!("executing: {opcode:?}");
                match opcode {
                    VmOp::AllocList(_) => todo!(),
                    VmOp::BuildAttrset => todo!(),
                    VmOp::LoadContext(_) => todo!(),
                    VmOp::LoadLocalThunk(idx) => {
                        let thunk = self.state.thunk_stack[idx as usize].clone();
                        let value = self.evaluator.get_evaluator().force_thunk(thunk)?;
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
                        let thunk_buf = &mut self.evaluator.thunk_alloc_buffer;
                        thunk_buf.clear();
                        for insn in self
                            .evaluator
                            .gc_handle
                            .load(&args.context_build_instructions)
                            .as_ref()
                        {
                            match insn {
                                crate::vm::opcodes::ValueSource::ContextReference(ctxref) => {
                                    thunk_buf.push(self.state.context[(*ctxref) as usize].clone())
                                }
                                crate::vm::opcodes::ValueSource::ThunkStackRef(_) => todo!(),
                            }
                        }
                        let context = ExecutionContext {
                            entries: self.evaluator.gc_handle.alloc_vec(thunk_buf)?,
                        };
                        let new_thunk = self
                            .evaluator
                            .gc_handle
                            .alloc(Thunk::Deferred { context, code })?;
                        let dest_idx = self.state.thunk_stack.len() - 1 - slot as usize;
                        self.state.thunk_stack[dest_idx] = new_thunk;
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
                        self.state.local_stack.push(execute_arithmetic_op(
                            left,
                            right,
                            |l, r| l + r,
                            |l, r| l + r,
                        )?);
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
                            NixValue::Bool(b) => NixValue::Bool(b),
                            _ => return Err(EvaluateError::TypeError),
                        };
                        self.state.local_stack.push(result);
                    }
                    VmOp::Call => todo!(),
                    VmOp::CastToPath => todo!(),
                    VmOp::PushImmediate(imm) => {
                        let imm = self.evaluator.gc_handle.load(&imm);
                        self.state.local_stack.push(imm.clone());
                    }
                    VmOp::CompareEqual => {
                        let right = self.pop()?;
                        let left = self.pop()?;
                        let result = Some(Ordering::Equal)
                            == compare_values(self.evaluator.gc_handle, &left, &right);
                        self.state.local_stack.push(NixValue::Bool(result));
                    }
                    VmOp::CompareNotEqual => {
                        let right = self.pop()?;
                        let left = self.pop()?;
                        let result = Some(Ordering::Equal)
                            != compare_values(self.evaluator.gc_handle, &left, &right);
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
                    VmOp::GetAttribute { push_error: _ } => todo!(),
                    VmOp::ConcatStrings(_) => todo!(),
                }
            }
        }

        self.state.code_buf = code_buf;
        let result_value = self.pop()?;

        println!("thunk evaluated to {result_value:?}");

        let value_thunk = self
            .evaluator
            .gc_handle
            .alloc(Thunk::Value(result_value.clone()))?;
        self.evaluator.gc_handle.replace(&thunk, value_thunk);
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
        self.evaluator
            .stack_cache
            .push(core::mem::take(&mut self.state));
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

fn compare_values(gc_handle: &GcHandle, l: &NixValue, r: &NixValue) -> Option<Ordering> {
    match (l, r) {
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
        (NixValue::Path(l), NixValue::Path(r)) => todo!(),
        (NixValue::Attrset(l), NixValue::Attrset(r)) => todo!(),
        (NixValue::List(_), NixValue::List(_)) => todo!(),
        _ => None,
    }
}
