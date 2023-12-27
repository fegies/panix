// mod parser;
mod lexer;

use std::{fs::File, io::Read, path::PathBuf};

use clap::Parser;

#[derive(Parser, Debug)]
struct Args {
    input_file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let mut file = File::open(&args.input_file).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let tokens = lexer::lex_input(buf.as_ref()).unwrap();
    println!("{tokens:?}");
    // parser::parse_nix(&mmap);
}
