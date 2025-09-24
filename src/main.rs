use std::{env, fs};

use rust_learning_language as rll;

use rll::{interpreter::Interpreter, repl};

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    match &args[..] {
        [] => repl::execute().unwrap(),
        [path] => {
            if let Ok(test_feature) = env::var("RLL_TEST") {
                match &test_feature[..] {
                    "no_copy" => test_no_copy(path),
                    "span_only" => test_span_only_lexer(path),
                    name => {
                        println!("Unknown feature to test: {name}");
                        return;
                    }
                }
            } else {
                run_file(path);
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

fn test_no_copy(path: &str) {
    use rll::lexers::no_copy_lexer::lex;
    let source = fs::read_to_string(path).expect("Failed to read code file.");
    let tokens = lex(&source);

    //use rll::lexers::no_copy_lexer::Token;
    //for token in tokens {
    //    match token {
    //        Ok(Token { kind, span, .. }) => println!("{kind:?} ({span})"),
    //        Err(err) => println!("ERROR: {err}"),
    //    }
    //}

    use rll::parsers::no_copy_parser::parse;
    for stmt in parse(tokens) {
        match stmt {
            Ok(stmt) => println!("{stmt:#?}"),
            Err(err) => println!("ERROR: {err}"),
        }
    }
}

fn test_span_only_lexer(path: &str) {
    use rll::lexers::span_only_lexer::{Spanned, lex};
    let source = fs::read_to_string(path).expect("Failed to read code file.");
    let tokens = lex(source.chars());
    for token in tokens {
        match token {
            Ok(Spanned { node: token, .. }) => println!("{token:?}"),
            Err(err) => println!("ERROR: {err}"),
        }
    }
}
