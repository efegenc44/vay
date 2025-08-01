use std::{collections::HashMap, fs};

use crate::{
    checker::Checker, declaration::Module, interner::Interner, interpreter::Interpreter, lexer::Lexer, parser::Parser, resolver::Resolver
};

macro_rules! runner_error {
    ($self:expr, $e:expr, $stage:literal) => {
        match $e {
            Ok(result) => {
                println!(" done.");
                result
            }
            Err(error) => {
                println!(" failed.");
                error.report(
                    &$self.source_contents[error.source()],
                    $stage,
                    &$self.interner
                );
                return Err(())
            }
        }
    };
}

pub struct Runner {
    interner: Interner,
    source_contents: HashMap<String, String>
}

impl Runner {
    pub fn new() -> Self {
        Self {
            interner: Interner::new(),
            source_contents: HashMap::new(),
        }
    }

    pub fn parse_all(&mut self, sources: Vec<String>) -> Result<Vec<Module>, ()> {
        let mut modules = vec![];
        for source in sources {
            // TODO: Better error reporting here
            let source_content = fs::read_to_string(&source)
                .map_err(|error| println!("\n    Error {}: {}", source, error))?;
            let lexer = Lexer::new(source.clone(), &source_content, &mut self.interner);
            let mut parser = Parser::new(lexer);
            print!("{:>20}: {}", "Parsing", source);
            let module = runner_error!(self, parser.module(), "parsing");
            modules.push(module);
            self.source_contents.insert(source, source_content);
        }

        Ok(modules)
    }

    pub fn check(&mut self, modules: Vec<Module>) -> Result<Vec<Module>, ()> {
        let mut resolver = Resolver::new();
        print!("{:>20}:", "Name Resolution");
        let modules = runner_error!(self, resolver.resolve(modules), "name resolution");
        let mut checker = Checker::new();
        print!("{:>20}:", "Type Checking");
        runner_error!(self, checker.type_check(&modules), "type checking");
        Ok(modules)
    }

    pub fn interpret(&self, modules: &[Module]) {
        let mut interpreter = Interpreter::new();
        print!("{:>20}\n", "Interpreting");
        interpreter.evaluate_main(modules, &self.interner);
    }
}