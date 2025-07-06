use std::{collections::HashMap, env, fs, io};

use crate::{error::Error, interner::Interner, lexer::Lexer, parser::Parser, resolver::Resolver};

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
mod error;

fn main() -> io::Result<()> {
    let mut interner = Interner::new();


    let mut modules = vec![];
    let mut sources = HashMap::new();
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
                error.print(&source_path, &source, "parsing");
                return Ok(());
            }
        };

        modules.push((program, source_path.clone()));
        sources.insert(source_path, source);
    }

    let mut resolver = Resolver::new();

    let _modules = match resolver.resolve(modules) {
        Ok(modules) => {
            println!("Name Resolution: OK");
            modules
        },
        Err((error, source_path)) => {
            error.print(&source_path, &sources[&source_path], "name resolution");
            println!("{interner:?}");
            return Ok(());
        }
    };


    Ok(())
}
