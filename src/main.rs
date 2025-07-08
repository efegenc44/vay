use std::{collections::HashMap, env, fs, io};

use crate::{interner::Interner, lexer::Lexer, parser::Parser, resolver::Resolver};

mod bound;
mod declaration;
mod expression;
mod interner;
mod lexer;
mod location;
mod parser;
mod reportable;
mod resolver;
mod statement;
mod token;

fn main() -> io::Result<()> {
    let mut interner = Interner::new();

    let mut modules = vec![];
    let mut source_contents = HashMap::new();
    for source in env::args().skip(1) {
        let source_content = fs::read_to_string(&source)?;

        let lexer = Lexer::new(source.clone(), &source_content, &mut interner);
        let mut parser = Parser::new(lexer);

        let module = match parser.module() {
            Ok(module) => {
                println!("{source} - Parsing: OK");
                module
            }
            Err(error) => {
                error.report(&source_content, "parsing", &interner);
                return Ok(());
            }
        };

        modules.push(module);
        source_contents.insert(source, source_content);
    }

    let mut resolver = Resolver::new();

    let _modules = match resolver.resolve(modules) {
        Ok(modules) => {
            println!("Name Resolution: OK");
            modules
        }
        Err(error) => {
            error.report(
                &source_contents[error.source()],
                "name resolution",
                &interner,
            );
            return Ok(());
        }
    };

    Ok(())
}
