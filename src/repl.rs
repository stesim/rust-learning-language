use std::io::{self, Write};

use crate::{
    interpreter::{Interpreter, Value},
    lexer::lex,
    parser::parse,
};

pub fn execute() -> Result<(), io::Error> {
    let mut interpreter = Interpreter::new();

    loop {
        let mut input = prompt_input()?;
        if input == "exit" {
            break;
        }

        // HACK: append semicolon to input to make expressions valid statements
        if !input.ends_with(';') {
            input.push(';');
        }

        match evaluate_input(&mut interpreter, &input) {
            Ok(Value::Unit) => {}
            Ok(value) => println!("{value}"),
            Err(err) => println!("ERROR: {err}"),
        }
    }

    Ok(())
}

pub fn evaluate_input(interpreter: &mut Interpreter, input: &str) -> Result<Value, String> {
    let tokens = lex(&input);
    let ast = parse(tokens).map_err(|e| e.to_string())?;

    interpreter.eval(ast).map_err(|e| e.to_string())
}

pub fn prompt_input() -> Result<String, io::Error> {
    print!("rll> ");
    io::stdout().flush()?;
    read_input()
}

pub fn read_input() -> Result<String, io::Error> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(input.trim().to_owned())
}
