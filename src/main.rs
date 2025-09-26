use std::{env, fs};

use rust_learning_language as rll;

fn main() {
    let eval = eval();
    let args: Vec<String> = env::args().skip(1).collect();
    match &args[..] {
        [] => rll::repl::run(eval).unwrap(),
        [path] => run_file(eval, path),
        _ => println!("Expected 0 or 1 arguments, got {}", args.len()),
    }
}

fn eval() -> impl FnMut(&str) -> Result<Option<String>, String> {
    use rll::{Interpreter, Value, lex, parse_with_interns};

    let mut interpreter = Interpreter::new();

    return move |input: &str| {
        let tokens = lex(&input);
        let ast =
            parse_with_interns(tokens, &mut interpreter.interns).map_err(|e| e.to_string())?;
        match interpreter.eval(&ast) {
            Ok(Value::Unit) => Ok(None),
            Ok(value) => Ok(Some(value.to_string())),
            Err(err) => Err(err.to_string()),
        }
    };
}

fn run_file(mut eval: impl FnMut(&str) -> Result<Option<String>, String>, path: &str) {
    let source = fs::read_to_string(path).expect("Failed to read code file.");
    match eval(&source) {
        Ok(None) => {}
        Ok(Some(value)) => println!("{value}"),
        Err(err) => println!("ERROR: {err}"),
    }
}
