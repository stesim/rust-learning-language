use std::io::{self, Write};

use crate::{
    lexer::lex,
    parser::parse,
    interpreter::{Interpreter, Value},
};

pub fn execute() -> Result<(), io::Error> {
    let mut interpreter = Interpreter::new();

    loop {
        let input = prompt_input()?;
        if input == "exit" {
            break;
        }

        let eval_result = evaluate_input(&mut interpreter, &input);
        if let Err(err) = eval_result {
            println!("ERROR: {err}");
        }
    }

    Ok(())
}

pub fn evaluate_input(interpreter: &mut Interpreter, input: &str) -> Result<Value, String> {
    let tokens = lex(&input);
    let ast = parse(tokens).map_err(|e| e.to_string())?;

    interpreter.eval(ast)
        .map(|_| Value::Unit)
        .map_err(|e| e.to_string())
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
