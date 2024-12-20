#![allow(dead_code, unused)]
mod compiler;
pub mod util;
mod vm;

pub mod evaluator;
pub use evaluator::Evaluator;

#[cfg(test)]
mod tests;

use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

use compiler::CompileError;
use gc::{GcError, GcHandle};
use parser::ParseError;
use vm::value::Thunk;

#[derive(Debug, thiserror::Error)]
pub enum EvaluateError {
    #[error("Infinite Recursion detected")]
    BlackholeEvaluated,
    #[error("The execution stack was emptied unexpectedly")]
    ExecutionStackExhaustedUnexpectedly,
    #[error("Error allocating memory")]
    GcError(#[from] GcError),
    #[error("Type error during evaluation")]
    TypeError,
    #[error("Key conflict in attribute set")]
    DuplicateAttrsetKey,
    #[error("the provided attrest key could not be found")]
    AttrsetKeyNotFound,

    #[error("Lambda called with unexpected argument: `{arg_name}`")]
    CallWithUnexpectedArg { arg_name: String },

    #[error("Lambda called with missing argument: `{arg_name}`")]
    CallWithMissingArg { arg_name: String },
}

#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("error while parsing")]
    ParseError(#[from] ParseError),
    #[error("IO error")]
    IoError(#[from] io::Error),
    #[error("Error compiling parsed AST:")]
    CompileError(#[from] CompileError),
    #[error("Error evaluating code:")]
    EvaluateError(#[from] EvaluateError),
}

pub fn compile_file(gc_handle: &mut GcHandle, file: &Path) -> Result<Thunk, InterpreterError> {
    let mut content = Vec::new();
    File::open(file)?.read_to_end(&mut content)?;
    compile_source(gc_handle, &content)
}

pub fn compile_source(gc_handle: &mut GcHandle, content: &[u8]) -> Result<Thunk, InterpreterError> {
    let expr = parser::parse_nix(content)?;
    let res = compiler::translate_expression(gc_handle, expr)?;
    Ok(res)
}
