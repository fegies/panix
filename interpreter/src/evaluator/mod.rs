use std::cmp::Ordering;

use gc::{GcHandle, GcPointer};

use crate::{
    vm::{
        opcodes::VmOp,
        value::{NixValue, Thunk},
    },
    EvaluateError,
};

use super::InterpreterError;

pub struct Evaluator<'gc> {
    gc_handle: &'gc mut GcHandle,
    execution_stack: Vec<Thunk>,
}

impl<'gc> Evaluator<'gc> {
    pub fn new(gc_handle: &'gc mut GcHandle) -> Self {
        Self {
            gc_handle,
            execution_stack: Vec::new(),
        }
    }
    pub fn eval_expression(&mut self, mut thunk: Thunk) -> Result<NixValue, InterpreterError> {
        Ok(self.force_thunk(&mut thunk)?)
    }

    fn force_thunk(&mut self, thunk: &mut Thunk) -> Result<NixValue, EvaluateError> {
        loop {
            match thunk {
                Thunk::Blackhole => return Err(EvaluateError::BlackholeEvaluated),
                Thunk::Value(v) => return Ok(v.clone()),
                Thunk::Deferred { context, code } => {
                    let mut code_vec: Vec<_> = Vec::new();
                    code_vec.extend_from_slice(self.gc_handle.load(&code).as_ref());
                    let mut code = code_vec.into_iter();
                    let context = context.clone();
                    *thunk = Thunk::Blackhole;

                    while let Some(opcode) = code.next() {
                        match opcode {
                            VmOp::AllocList(_) => todo!(),
                            VmOp::BuildAttrset => todo!(),
                            VmOp::LoadContext(_) => todo!(),
                            VmOp::AllocateThunk {
                                context_length,
                                code,
                            } => todo!(),
                            VmOp::Skip(_) => todo!(),
                            VmOp::SkipUnless(_) => todo!(),
                            VmOp::ConcatLists(_) => todo!(),
                            VmOp::Add => {
                                let right = self.pop_and_force()?;
                                let left = self.pop_and_force()?;
                                self.execution_stack
                                    .push(Thunk::Value(execute_arithmetic_op(
                                        left,
                                        right,
                                        |l, r| l + r,
                                        |l, r| l + r,
                                    )?));
                            }
                            VmOp::Mul => {
                                let right = self.pop_and_force()?;
                                let left = self.pop_and_force()?;
                                self.execution_stack
                                    .push(Thunk::Value(execute_arithmetic_op(
                                        left,
                                        right,
                                        |l, r| l * r,
                                        |l, r| l * r,
                                    )?));
                            }
                            VmOp::Div => {
                                let right = self.pop_and_force()?;
                                let left = self.pop_and_force()?;
                                self.execution_stack
                                    .push(Thunk::Value(execute_arithmetic_op(
                                        left,
                                        right,
                                        |l, r| l / r,
                                        |l, r| l / r,
                                    )?));
                            }
                            VmOp::NumericNegate => {
                                let result = match self.pop_and_force()? {
                                    NixValue::Int(i) => NixValue::Int(-i),
                                    NixValue::Float(f) => NixValue::Float(-f),
                                    _ => return Err(EvaluateError::TypeError),
                                };
                                self.execution_stack.push(Thunk::Value(result));
                            }
                            VmOp::BinaryNot => {
                                let result = match self.pop_and_force()? {
                                    NixValue::Bool(b) => NixValue::Bool(b),
                                    _ => return Err(EvaluateError::TypeError),
                                };
                                self.execution_stack.push(Thunk::Value(result));
                            }
                            VmOp::Call => todo!(),
                            VmOp::CastToPath => todo!(),
                            VmOp::PushImmediate(imm) => {
                                let imm = self.gc_handle.load(&imm);
                                self.execution_stack.push(Thunk::Value(imm.clone()));
                            }
                            VmOp::CompareEqual => {
                                let right = self.pop_and_force()?;
                                let left = self.pop_and_force()?;
                                let result = Some(Ordering::Equal)
                                    == compare_values(self.gc_handle, &left, &right);
                                self.execution_stack
                                    .push(Thunk::Value(NixValue::Bool(result)));
                            }
                            VmOp::CompareNotEqual => {
                                let right = self.pop_and_force()?;
                                let left = self.pop_and_force()?;
                                let result = Some(Ordering::Equal)
                                    != compare_values(self.gc_handle, &left, &right);
                                self.execution_stack
                                    .push(Thunk::Value(NixValue::Bool(result)));
                            }
                            VmOp::Sub => {
                                let right = self.pop_and_force()?;
                                let left = self.pop_and_force()?;
                                self.execution_stack
                                    .push(Thunk::Value(execute_arithmetic_op(
                                        left,
                                        right,
                                        |l, r| l - r,
                                        |l, r| l - r,
                                    )?));
                            }
                        }
                    }

                    *thunk = self.pop()?;
                }
            }
        }
    }

    fn pop(&mut self) -> Result<Thunk, EvaluateError> {
        self.execution_stack
            .pop()
            .ok_or(EvaluateError::ExecutionStackExhaustedUnexpectedly)
    }
    fn pop_and_force(&mut self) -> Result<NixValue, EvaluateError> {
        let mut thunk = self.pop()?;
        self.force_thunk(&mut thunk)
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
