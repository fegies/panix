mod compiler;
pub mod util;
mod vm;

use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

use compiler::CompileError;
use gc::GcHandle;
use parser::ParseError;
use vm::value::Thunk;

#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("error while parsing")]
    ParseError(#[from] ParseError),
    #[error("IO error")]
    IoError(#[from] io::Error),
    #[error("Error compiling parsed AST:")]
    CompileError(#[from] CompileError),
}

pub fn compile_file(gc_handle: &mut GcHandle, file: &Path) -> Result<Thunk, InterpreterError> {
    let mut content = Vec::new();
    File::open(file)?.read_to_end(&mut content)?;
    let expr = parser::parse_nix(&content)?;
    let res = compiler::translate_expression(gc_handle, expr)?;
    println!("{res:#?}");
    Ok(res)
}
