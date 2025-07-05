use std::{env, fs, io};

use crate::{interner::Interner, lexer::Lexer, parser::Parser, resolver::Resolver};

mod bound;
mod declaration;
mod expression;
mod interner;
mod lexer;
mod location;
mod parser;
mod resolver;
mod statement;
mod token;

fn main() -> io::Result<()> {
    let mut interner = Interner::new();

    let mut modules = vec![];
    for source_path in env::args().skip(1) {
        let source = fs::read_to_string(&source_path)?;

        let lexer = Lexer::new(&source, &mut interner);
        let mut parser = Parser::new(lexer);

        let program = match parser.program() {
            Ok(program) => {
                println!("{source_path} - Parsing: OK");
                program
            }
            Err(error) => {
                println!("{source_path} - Parsing: {error}");
                return Ok(());
            }
        };

        modules.push(program);
    }

    let mut resolver = Resolver::new();

    let _modules = match resolver.resolve(modules) {
        Ok(modules) => {
            println!("Name Resolution: OK");
            modules
        },
        Err(error) => {
            println!("Name Resolution: {error}");
            println!("{interner:?}");
            return Ok(());
        }
    };


    Ok(())
}
