use std::{env, fs};

use rust_learning_language as rll;

fn main() {
    let eval_type = env::var("RLL_EVAL").unwrap_or("simple".to_string());
    match &eval_type[..] {
        "simple" => run(simple_eval()),
        "no_copy" => run(no_copy_eval()),
        "interning" => run(interning_eval()),
        _ => println!("Unknown eval type."),
    };
}

fn run(eval: impl FnMut(&str) -> Result<Option<String>, String>) {
    let args: Vec<String> = env::args().skip(1).collect();
    match &args[..] {
        [] => {
            rll::repl::run(eval).unwrap();
        }
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
                run_file(eval, path);
            }
        }
        _ => println!("Expected 0 or 1 arguments, got {}", args.len()),
    }
}

fn simple_eval() -> impl FnMut(&str) -> Result<Option<String>, String> {
    use rll::{
        interpreters::simple_interpreter::{Interpreter, Value},
        lexers::simple_lexer::lex,
        parsers::simple_parser::parse,
    };

    let mut interpreter = Interpreter::new();

    return move |input: &str| {
        let tokens = lex(&input);
        let ast = parse(tokens).map_err(|e| e.to_string())?;
        match interpreter.eval(ast) {
            Ok(Value::Unit) => Ok(None),
            Ok(value) => Ok(Some(value.to_string())),
            Err(err) => Err(err.to_string()),
        }
    };
}

fn no_copy_eval() -> impl FnMut(&str) -> Result<Option<String>, String> {
    use rll::{
        interpreters::no_copy_interpreter::{Interpreter, Value},
        lexers::no_copy_lexer::lex,
        parsers::no_copy_parser::parse,
    };

    let mut interpreter = Interpreter::new();

    return move |input: &str| {
        // HACK
        let input: &'static str = Box::leak(input.to_string().into_boxed_str());

        let tokens = lex(input);

        // HACK
        let ast: &'static Vec<_> = Box::leak(Box::new(
            parse(tokens)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| e.to_string())?,
        ));

        match interpreter.eval(ast.iter()) {
            Ok(Value::Unit) => Ok(None),
            Ok(value) => Ok(Some(value.to_string())),
            Err(err) => Err(err.to_string()),
        }
    };
}

fn interning_eval() -> impl FnMut(&str) -> Result<Option<String>, String> {
    use rll::{
        interpreters::interning_interpreter::{Interpreter, Value},
        lexers::no_copy_lexer::lex,
        parsers::interning_parser::parse_with_interns,
    };

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
