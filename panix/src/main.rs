// mod parser;

use std::{fs::File, io::Read, path::PathBuf, time::Duration};

use clap::Parser;
use parser::lexer;

#[derive(Parser, Debug)]
struct Args {
    input_file: PathBuf,
}

fn main() {
    let args = Args::parse();

    gc::init();

    let mut file = File::open(&args.input_file).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let tokens = lexer::lex_input(buf.as_ref()).unwrap();
    println!("{tokens:?}");
    println!("{}", tokens.len());
}
