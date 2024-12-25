use std::sync::LazyLock;

use gc::{GcHandle, GcPointer};
use gc_derive::Trace;
use parser::{
    ast::{Code, LetInExpr, NixExprContent},
    parse_nix,
};

use crate::{
    evaluator,
    vm::value::{Attrset, NixValue, Thunk},
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
    let mut builtins = parse_nix(include_bytes!("./definition.nix"))
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
}

impl Builtins for NixBuiltins {
    type TypeToken = BuiltinTypeToken;
    type ExecuteError = EvaluateError;

    fn get_builtin(&self, ident: &str) -> Option<Self::TypeToken> {
        let t = match ident {
            "___builtin_throw" => BuiltinType::Throw,
            "___builtin_tryeval" => BuiltinType::TryEval,
            "___builtin_typeof" => BuiltinType::TypeOf,
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
        }
    }
}
