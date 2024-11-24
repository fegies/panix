// mod parser;

use std::path::{Path, PathBuf};

use clap::Parser;
use gc::with_gc;
use interpreter::Evaluator;

#[derive(Parser, Debug)]
struct Args {
    input_file: PathBuf,
}

fn process_file(file: &Path) {
    println!("parsing: {file:?}");

    with_gc(|handle| {
        let thunk = interpreter::compile_file(handle, file).unwrap();
        let result = Evaluator::new(handle).eval_expression(thunk);
        println!("{result:?}");
    })
    .unwrap();

    // let res = lexer::run(&buf, |it| it.collect::<Vec<_>>());

    // let tokens = lexer::lex_input(buf.as_ref()).unwrap();
    // println!("{tokens:?}");
    // println!("{}", tokens.len());
}

fn walk_dir(dir: &mut PathBuf) {
    for dirent in std::fs::read_dir(&dir).unwrap() {
        let dirent = dirent.unwrap();

        let name = dirent.file_name();
        dir.push(&name);

        if name.to_string_lossy().ends_with(".nix") {
            process_file(&dir);
        } else if dirent.metadata().unwrap().is_dir() {
            walk_dir(dir);
        }
        dir.pop();
    }
}

fn main() {
    let args = Args::parse();

    let mut input = args.input_file;
    let meta = std::fs::metadata(&input).unwrap();
    if meta.is_dir() {
        walk_dir(&mut input);
    } else {
        process_file(&input);
    }
}
