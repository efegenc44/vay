use std::{collections::HashMap, env, fs, io};

use crate::{
    checker::Checker, interner::Interner, interpreter::Interpreter, lexer::Lexer, parser::Parser, resolver::Resolver
};

mod bound;
mod checker;
mod declaration;
mod expression;
mod interner;
mod lexer;
mod location;
mod parser;
mod reportable;
mod resolver;
mod token;
mod typ;
mod interpreter;
mod value;

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
                println!("Parsing: OK - {source}");
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

    let modules = match resolver.resolve(modules) {
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

    let mut checker = Checker::new();
    match checker.type_check(&modules) {
        Ok(()) => println!("Type Checking: OK"),
        Err(error) => {
            error.report(&source_contents[error.source()], "type checking", &interner);
            return Ok(());
        }
    }

    let mut interpreter = Interpreter::new();
    interpreter.evaluate_main(&modules, &interner);

    Ok(())
}
