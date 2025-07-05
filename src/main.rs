use std::{env, fs, io};

use crate::{interner::Interner, lexer::Lexer, parser::Parser};

mod declaration;
mod expression;
mod interner;
mod lexer;
mod location;
mod parser;
mod statement;
mod token;

fn main() -> io::Result<()> {
    let source = match &env::args().collect::<Vec<_>>()[1..] {
        [source_path] => fs::read_to_string(source_path)?,
        _ => todo!("ERROR: Source path is not provided."),
    };

    let lexer = Lexer::new(&source, Interner::new());
    let mut parser = Parser::new(lexer);

    match parser.module() {
        Ok(_) => println!("OK"),
        Err(error) => println!("{error}"),
    }

    Ok(())
}
