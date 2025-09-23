use std::{env, fs};

use rust_learning_language::{interpreter::Interpreter, repl};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    match &args[..] {
        [] => repl::execute().unwrap(),
        [path] => run_file(path),
        _ => println!("Expected 0 or 1 arguments, got {}", args.len()),
    }
}

fn run_file(path: &str) {
    // NOTE: the function uses the REPL implementation to evaluate the given file to avoid
    //       duplicating the lexing, parsing, interpreting and error handling steps
    let source = fs::read_to_string(path).expect("Failed to read code file.");
    let mut interpreter = Interpreter::new();
    match repl::evaluate_input(&mut interpreter, &source) {
        Err(err) => println!("ERROR: {err}"),
        _ => {}
    };
}
