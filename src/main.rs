#[macro_use]
mod errors;
mod lexer;
mod parser;
mod tokens;
mod utils;

use crate::lexer::Lexer;
use crate::tokens::Delimiter;
use errors::Result;
use std::io::{stdin, stdout, Write};
use std::process::exit;

fn try_main() -> Result<()> {
    loop {
        let mut buffer = String::new();
        const FILE: &str = "CASSIOPEIA-REPL";

        print!("{}> ", FILE);

        stdout().flush().unwrap();

        if let Err(_) = stdin().read_line(&mut buffer) {
            continue;
        }

        buffer = buffer.trim().to_string();

        match Lexer::new(&buffer)
            .file(FILE)
            .operators(&[
                "+", "-", "*", "/", "+.", "-.", "*.", "/.", "<>", ":", "->", "==", "<=", "<", ">",
                ">=", ",", "&&", "||", "^^", ":-",
            ])
            .delimiters(&[
                (Delimiter::Bracket, '[', ']'),
                (Delimiter::Paren, '(', ')'),
                (Delimiter::Brace, '{', '}'),
            ])
            .keywords(&[
                "let", "fun", "if", "then", "else", "match", "integer", "float", "string", "char",
                "bool", "true", "false",
            ])
            .lex()
        {
            Ok(t) => t.into_iter().for_each(|x| println!("{:?}", x)),
            Err(e) => eprintln!("{}", e),
        }

        buffer.clear();
    }
}

fn main() {
    match try_main() {
        Ok(_) => exit(0),
        Err(e) => eprintln!("{}", e),
    }

    exit(1)
}
