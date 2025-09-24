use std::{env, fs};

use rust_learning_language::{interpreter::Interpreter, repl};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    match &args[..] {
        [] => repl::execute().unwrap(),
        [path] => {
            if env::var("TEST_NO_COPY").is_ok() {
                test_no_copy_lexer(path)
            } else {
                run_file(path)
            }
        }
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

fn test_no_copy_lexer(path: &str) {
    use rust_learning_language::no_copy_lexer::{Spanned, lex};
    let source = fs::read_to_string(path).expect("Failed to read code file.");
    let tokens = lex(&source);
    for token in tokens {
        match token {
            Ok(Spanned { node: token, .. }) => println!("{token:?}"),
            Err(err) => println!("ERROR: {err}"),
        }
    }
}
