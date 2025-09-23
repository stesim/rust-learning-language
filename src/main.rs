use std::fs;

use rust_learning_language::{lexer, parser};

fn main() {
    let source = fs::read_to_string("example.rll").expect("Failed to read code file.");

    let tokens = lexer::lex(&source);
    println!("Tokens {:#?}", tokens);

    parser::parse(tokens)
        .inspect(|ast| println!("{:#?}", ast))
        .inspect_err(|err| println!("{}", err))
        .ok();
}
