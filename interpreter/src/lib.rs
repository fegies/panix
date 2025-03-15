mod compiler;
pub mod util;
mod vm;

mod builtins;
pub mod evaluator;
use builtins::{NixBuiltins, get_builtins};
use bumpalo::Bump;
pub use evaluator::Evaluator;

#[cfg(test)]
mod tests;

use std::{
    error::Error,
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
    AttrsetKeyNotFound { attr_name: String },

    #[error("Lambda called with unexpected argument: `{arg_name}`")]
    CallWithUnexpectedArg { arg_name: String },

    #[error("Lambda called with missing argument: `{arg_name}`")]
    CallWithMissingArg { arg_name: String },

    #[error("The code threw an exception")]
    Throw { value: String },
    #[error("evaluation aborted")]
    Abort { value: String },

    #[error("The index is of of the valid range")]
    AccessOutOfRange,

    #[error("{msg}")]
    TypeErrorWithMessage { msg: String },

    #[error("error while importing {0}")]
    ImportError(Box<InterpreterError>),

    #[error("other error: {0}")]
    Misc(Box<dyn Error>),

    #[error("error performing io: {0}")]
    IO(#[from] std::io::Error),
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
    let source_filename = file.to_str().unwrap();
    compile_source(gc_handle, &content, source_filename)
}

fn compile_source_with_nix_filename(
    gc_handle: &mut GcHandle,
    content: &[u8],
    source_filename: crate::vm::value::NixString,
    builtins: &NixBuiltins,
) -> Result<Thunk, InterpreterError> {
    let expr = parser::parse_nix(content)?;
    let bump = Bump::new();
    let res = compiler::translate_expression(gc_handle, expr, &bump, source_filename, builtins)?;
    Ok(res)
}

pub fn compile_source(
    gc_handle: &mut GcHandle,
    content: &[u8],
    source_filename: &str,
) -> Result<Thunk, InterpreterError> {
    let source_filename = gc_handle
        .alloc_string(source_filename)
        .map_err(|e| CompileError::Gc(e))?
        .into();

    let builtins = get_builtins(gc_handle).map_err(|e| CompileError::Gc(e))?;

    compile_source_with_nix_filename(gc_handle, content, source_filename, &builtins)
}
