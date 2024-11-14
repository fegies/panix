use std::cmp::Ordering;

use gc::{GcHandle, GcPointer};

use crate::{
    vm::value::{NixValue, Thunk},
    EvaluateError,
};

use super::InterpreterError;

pub struct Evaluator<'gc> {
    gc_handle: &'gc mut GcHandle,
    execution_stack: Vec<GcPointer<NixValue>>,
}

impl<'gc> Evaluator<'gc> {
    pub fn new(gc_handle: &'gc mut GcHandle) -> Self {
        Self {
            gc_handle,
            execution_stack: Vec::new(),
        }
    }
    pub fn eval_expression(
        &mut self,
        mut thunk: Thunk,
    ) -> Result<GcPointer<NixValue>, InterpreterError> {
        Ok(self.force_thunk(&mut thunk)?)
    }

    fn force_thunk(&mut self, thunk: &mut Thunk) -> Result<GcPointer<NixValue>, EvaluateError> {
        match thunk {
            Thunk::Blackhole => Err(EvaluateError::BlackholeEvaluated),
            Thunk::Value(v) => Ok(v.clone()),
            Thunk::Deferred { context, code } => {
                let mut code_vec: Vec<_> = Vec::new();
                code_vec.extend_from_slice(self.gc_handle.load(&code).as_ref());
                let mut code = code_vec.into_iter();

                while let Some(opcode) = code.next() {
                    match opcode {
                        crate::vm::opcodes::VmOp::AllocList(_) => todo!(),
                        crate::vm::opcodes::VmOp::BuildAttrset => todo!(),
                        crate::vm::opcodes::VmOp::LoadContext(_) => todo!(),
                        crate::vm::opcodes::VmOp::PushImmediate(imm) => {
                            self.execution_stack.push(imm)
                        }
                        crate::vm::opcodes::VmOp::Add => todo!(),
                        crate::vm::opcodes::VmOp::Sub => todo!(),
                        crate::vm::opcodes::VmOp::AllocateThunk {
                            context_length,
                            code,
                        } => todo!(),
                        crate::vm::opcodes::VmOp::Skip(_) => todo!(),
                        crate::vm::opcodes::VmOp::SkipConditional(_) => todo!(),
                        crate::vm::opcodes::VmOp::ConcatLists(_) => todo!(),
                        crate::vm::opcodes::VmOp::NumericNegate => todo!(),
                        crate::vm::opcodes::VmOp::BinaryNot => todo!(),
                        crate::vm::opcodes::VmOp::Call => todo!(),
                        crate::vm::opcodes::VmOp::CastToPath => todo!(),
                        crate::vm::opcodes::VmOp::Equals => {
                            let right = self.pop()?;
                            let left = self.pop()?;
                            let result = match compare_values(
                                self.gc_handle.load(&left),
                                self.gc_handle.load(&right),
                            ) {
                                Some(Ordering::Equal) => true,
                                _ => false,
                            };
                            self.execution_stack
                                .push(self.gc_handle.alloc(NixValue::Bool(result))?);
                        }
                    }
                }

                let value = self.pop()?;
                *thunk = Thunk::Value(value.clone());
                Ok(value)
            }
        }
    }

    fn pop(&mut self) -> Result<GcPointer<NixValue>, EvaluateError> {
        self.execution_stack
            .pop()
            .ok_or(EvaluateError::ExecutionStackExhaustedUnexpectedly)
    }
}

fn compare_values(l: &NixValue, r: &NixValue) -> Option<Ordering> {
    match (l, r) {
        (NixValue::String(l), NixValue::String(r)) => todo!(),
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
