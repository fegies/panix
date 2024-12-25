use std::sync::LazyLock;

use gc::GcPointer;
use gc_derive::Trace;
use parser::{
    ast::{Code, LetInExpr, NixExprContent},
    parse_nix,
};

use crate::{
    vm::{
        opcodes::{ContextReference, ExecutionContext, ValueSource, VmOp},
        value::{self, Attrset, List, NixString, NixValue, Thunk},
    },
    EvaluateError, Evaluator,
};

pub trait Builtins {
    /// an opaque token that identifies the specific builtin being called.
    type TypeToken;
    type ExecuteError;

    /// try to resolve the given ident as a built in fuction.
    /// the provided token is guaranteed to be able to be used in the execute_builtin call.
    fn get_builtin(&self, ident: &str) -> Option<Self::TypeToken>;

    fn execute_builtin(
        &self,
        builtin: Self::TypeToken,
        argument: GcPointer<Thunk>,
        evaluator: &mut Evaluator,
    ) -> Result<NixValue, Self::ExecuteError>;
}

static BUILTINS_EXPR: LazyLock<LetInExpr<'static>> = LazyLock::new(build_builtins_expr);

pub fn get_builtins() -> NixBuiltins {
    NixBuiltins { _private: () }
}

pub fn get_builtins_expr() -> LetInExpr<'static> {
    BUILTINS_EXPR.clone()
}

fn build_builtins_expr() -> LetInExpr<'static> {
    let builtins = parse_nix(include_bytes!("./definition.nix"))
        .expect("to be able to parse the static builtin def");

    if let NixExprContent::Code(Code::LetInExpr(letexpr)) = builtins.content {
        letexpr
    } else {
        unreachable!("we built a let in expr.");
    }
}

#[derive(Clone)]
pub struct NixBuiltins {
    _private: (),
}

#[derive(Debug, Clone, Trace, Copy)]
pub struct BuiltinTypeToken {
    inner: BuiltinType,
}
#[derive(Debug, Clone, Trace, Copy)]
enum BuiltinType {
    Throw,
    TryEval,
    TypeOf,
    ToString,
    Map,
}

impl Builtins for NixBuiltins {
    type TypeToken = BuiltinTypeToken;
    type ExecuteError = EvaluateError;

    fn get_builtin(&self, ident: &str) -> Option<Self::TypeToken> {
        let t = match ident {
            "___builtin_throw" => BuiltinType::Throw,
            "___builtin_tryeval" => BuiltinType::TryEval,
            "___builtin_typeof" => BuiltinType::TypeOf,
            "___builtin_tostring" => BuiltinType::ToString,
            "___builtin_map" => BuiltinType::Map,
            _ => return None,
        };

        Some(BuiltinTypeToken { inner: t })
    }

    fn execute_builtin(
        &self,
        builtin: Self::TypeToken,
        argument: GcPointer<Thunk>,
        evaluator: &mut Evaluator,
    ) -> Result<NixValue, Self::ExecuteError> {
        match builtin.inner {
            BuiltinType::Throw => Err(EvaluateError::Throw {
                value: "todo".to_owned(),
            }),
            BuiltinType::TryEval => {
                let value = evaluator.force_thunk(argument).ok();
                let success_string = evaluator.gc_handle.alloc_string("success")?.into();
                let value_string = evaluator.gc_handle.alloc_string("value")?.into();

                let bool_value = NixValue::Bool(value.is_some());

                let value = evaluator
                    .gc_handle
                    .alloc(Thunk::Value(value.unwrap_or_else(|| bool_value.clone())))?;
                let bool_value = evaluator.gc_handle.alloc(Thunk::Value(bool_value))?;

                let entries = evaluator
                    .gc_handle
                    .alloc_slice(&[(success_string, bool_value), (value_string, value)])?;

                let attrset = Attrset { entries };

                Ok(NixValue::Attrset(attrset))
            }
            BuiltinType::TypeOf => {
                let typename = match evaluator.force_thunk(argument)? {
                    NixValue::String(_) => "string",
                    NixValue::Bool(_) => "bool",
                    NixValue::Null => "null",
                    NixValue::Int(_) => "int",
                    NixValue::Float(_) => "float",
                    NixValue::Path(_) => "path",
                    NixValue::Attrset(_) => "set",
                    NixValue::Function(_) => "lambda",
                    NixValue::List(_) => "list",
                    NixValue::Builtin(_) => "lambda",
                };

                let value = evaluator.gc_handle.alloc_string(typename)?;

                Ok(NixValue::String(value.into()))
            }
            BuiltinType::ToString => Ok(NixValue::String(execute_to_string(argument, evaluator)?)),
            BuiltinType::Map => execute_map(evaluator, argument),
        }
    }
}

fn execute_map(
    evaluator: &mut Evaluator<'_>,
    argument: GcPointer<Thunk>,
) -> Result<NixValue, EvaluateError> {
    if let NixValue::Attrset(argument) = evaluator.force_thunk(argument)? {
        let list = argument
            .get_entry_str(&evaluator.gc_handle, "list")
            .ok_or(EvaluateError::AttrsetKeyNotFound)?;
        let list = evaluator.force_thunk(list)?.expect_list()?;

        let mut list_entries = evaluator.gc_handle.load(&list.entries).as_ref().to_owned();

        if list_entries.is_empty() {
            // all empty lists are in principle identical.
            // no need to allocate anything more here.
            return Ok(NixValue::List(list));
        }

        let func = argument
            .get_entry_str(&evaluator.gc_handle, "func")
            .ok_or(EvaluateError::AttrsetKeyNotFound)?;

        // since the call op will pop the argument off the context stack,
        // we only need to ensure that the argument is on top of the stack.
        let call_code = evaluator.gc_handle.alloc_slice(&[
            VmOp::LoadContext(ContextReference(0)),
            VmOp::DuplicateThunk(ValueSource::ContextReference(1)),
            VmOp::Call,
        ])?;

        // we implement the map by replacing each source thunk pointer by
        // a newly allocated deferred thunk that models the call.
        for entry in list_entries.iter_mut() {
            let context = evaluator
                .gc_handle
                .alloc_slice(&[func.clone(), entry.clone()])?;
            let thunk = evaluator.gc_handle.alloc(Thunk::Deferred {
                context: ExecutionContext { entries: context },
                code: call_code.clone(),
            })?;
            *entry = thunk;
        }

        let entries = evaluator.gc_handle.alloc_vec(&mut list_entries)?;
        Ok(NixValue::List(List { entries }))
    } else {
        Err(EvaluateError::TypeError)
    }
}

fn execute_to_string(
    argument: GcPointer<Thunk>,
    evaluator: &mut Evaluator,
) -> Result<value::NixString, EvaluateError> {
    let result = match evaluator.force_thunk(argument)? {
        NixValue::String(s) => s,
        NixValue::Bool(b) => {
            let value = if b { "1" } else { "" };
            evaluator.gc_handle.alloc_string(value)?.into()
        }
        NixValue::Null => evaluator.gc_handle.alloc_string("")?.into(),
        NixValue::Int(int) => {
            let val = format!("{int}");
            evaluator.gc_handle.alloc_string(&val)?.into()
        }
        NixValue::Float(float) => {
            let val = format!("{float}");
            evaluator.gc_handle.alloc_string(&val)?.into()
        }
        NixValue::Path(path) => path,
        NixValue::Attrset(_attrset) => {
            // the cases where the attrset can be stringified is handled by the
            // nix code in the prelude
            return Err(EvaluateError::TypeErrorWithMessage {
                msg: format!("cannot coerce attrset to string"),
            });
        }
        NixValue::List(list) => {
            let entries = evaluator.gc_handle.load(&list.entries).as_ref().to_owned();
            if entries.len() == 0 {
                return Ok(evaluator.gc_handle.alloc_string("")?.into());
            }
            if entries.len() == 1 {
                return execute_to_string(entries.into_iter().next().unwrap(), evaluator);
            }

            let mut strings = Vec::with_capacity(entries.len() * 2);
            let separator: NixString = evaluator.gc_handle.alloc_string(" ")?.into();
            for entry in entries {
                strings.push(execute_to_string(entry, evaluator)?);
                strings.push(separator.clone());
            }

            // we pushed one final separator too much.
            strings.pop();

            NixString::concat_many(
                strings.into_iter().map(Ok::<_, EvaluateError>),
                &mut evaluator.gc_handle,
            )?
        }
        NixValue::Function(_) => {
            return Err(EvaluateError::TypeErrorWithMessage {
                msg: format!("connot coerce function to string"),
            })
        }
        NixValue::Builtin(_) => {
            return Err(EvaluateError::TypeErrorWithMessage {
                msg: format!("cannot coerce function to string"),
            })
        }
    };

    Ok(result)
}
