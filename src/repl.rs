use std::io::{self, Write};

pub fn run(mut eval: impl FnMut(&str) -> Result<Option<String>, String>) -> Result<(), io::Error> {
    loop {
        let mut input = prompt_input()?;
        if input == "exit" {
            break;
        }

        // HACK: append semicolon to input to make expressions valid statements
        if !input.ends_with(';') {
            input.push(';');
        }

        let result = eval(&input);
        display_result(result);
    }

    Ok(())
}

pub fn display_result(result: Result<Option<String>, String>) {
    match result {
        Ok(Some(value)) => println!("{value}"),
        Ok(None) => {}
        Err(err) => println!("ERROR: {err}"),
    }
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
