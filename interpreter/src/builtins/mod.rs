use std::sync::LazyLock;

use gc::GcHandle;
use gc_derive::Trace;
use parser::{
    ast::{Code, LetInExpr, NixExprContent},
    parse_nix,
};

use crate::{vm::value::NixValue, EvaluateError};

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
        argument: NixValue,
        gc: &mut GcHandle,
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
}

impl Builtins for NixBuiltins {
    type TypeToken = BuiltinTypeToken;
    type ExecuteError = EvaluateError;

    fn get_builtin(&self, ident: &str) -> Option<Self::TypeToken> {
        let t = match ident {
            "___builtin_throw" => BuiltinType::Throw,
            "___builtin_tryeval" => BuiltinType::TryEval,
            _ => return None,
        };

        Some(BuiltinTypeToken { inner: t })
    }

    fn execute_builtin(
        &self,
        builtin: Self::TypeToken,
        argument: NixValue,
        gc: &mut GcHandle,
    ) -> Result<NixValue, Self::ExecuteError> {
        match builtin.inner {
            BuiltinType::Throw => Err(EvaluateError::Throw {
                value: "todo".to_owned(),
            }),
            BuiltinType::TryEval => todo!(),
        }
    }
}
