// mod parser;

use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use clap::Parser;
use parser::parser::ParseError;

#[derive(Parser, Debug)]
struct Args {
    input_file: PathBuf,
}

fn process_file(file: &Path) {
    println!("parsing: {file:?}");
    let mut file = File::open(file).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let mut ok = false;
    match parser::parser::parse_nix(&buf) {
        Ok(res) => {
            println!("{res:#?}");
            ok = true;
        }
        Err(ParseError::UnexpectedToken(t)) => {
            println!("unexpected token: {t}");
        }
        Err(e) => {
            println!("{e:?}")
        }
    }
    assert!(ok);
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
    gc::init();

    let args = Args::parse();

    let mut input = args.input_file;
    let meta = std::fs::metadata(&input).unwrap();
    if meta.is_dir() {
        walk_dir(&mut input);
    } else {
        process_file(&input);
    }
}
