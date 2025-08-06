use core::panic;
use std::{collections::HashMap, fs, io::{stdin, stdout, Write}, process::exit};

use crate::{
    checker::Checker, declaration::Module, interner::Interner, interpreter::{ControlFlow, Interpreter}, intrinsics::INTRINSICS_FILE_PATH, lexer::Lexer, parser::Parser, reportable::ReportableResult, resolver::Resolver
};

pub const SESSION_SOURCE: &str = "Interactive session";

macro_rules! runner_handle {
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

macro_rules! runner_interactive {
    ($self:expr, $e:expr, $stage:literal) => {
        match $e {
            Ok(result) => {
                result
            }
            Err(error) => {
                error.report(
                    &$self.source_contents[error.source()],
                    $stage,
                    &$self.interner
                );
                continue
            }
        }
    };
}

pub struct Runner {
    source_contents: HashMap<String, String>,
    interner: Interner,
    resolver: Resolver,
    checker: Checker,
    interpreter: Interpreter
}

impl Runner {
    pub fn new() -> Self {
        Self {
            source_contents: HashMap::new(),
            interner: Interner::new(),
            resolver: Resolver::new(),
            checker: Checker::new(),
            interpreter: Interpreter::new(),
        }
    }

    pub fn parse_all(&mut self, mut sources: Vec<String>) -> Result<Vec<Module>, ()> {
        sources.push(INTRINSICS_FILE_PATH.into());

        let mut modules = vec![];
        for source in sources {
            // TODO: Better error reporting here
            let source_content = fs::read_to_string(&source)
                .map_err(|error| println!("\n    Error {}: {}", source, error))?;
            self.source_contents.insert(source.clone(), source_content);
            let source_content = &self.source_contents[&source];
            print!("{:>20}: {}", "Parsing", source);
            let lexer = Lexer::new(source, source_content, &mut self.interner);
            let mut parser = Parser::new(lexer);
            let module = runner_handle!(self, parser.module(), "parsing");
            modules.push(module);
        }

        Ok(modules)
    }

    pub fn check(&mut self, modules: Vec<Module>) -> Result<Vec<Module>, ()> {
        print!("{:>20}:", "Name Resolution");
        let modules = runner_handle!(self, self.resolver.resolve(modules, &self.interner), "name resolution");
        print!("{:>20}:", "Type Checking");
        runner_handle!(self, self.checker.type_check(&modules, &self.interner), "type checking");
        Ok(modules)
    }

    pub fn interpret(&mut self, modules: &[Module]) {
        let mut interpreter = Interpreter::new();
        println!("{:>20}", "Interpreting");
        interpreter.evaluate_main(modules, &self.interner);
    }

    pub fn interactive(&mut self, session_start_module_paths: Vec<String>) {
        let modules = self.parse_all(session_start_module_paths).unwrap();
        let modules = self.check(modules).unwrap();

        for module in modules {
            self.interpreter.collect_names(&module, &self.interner);
        }

        self.resolver.init_interactive_module();

        let stdin = stdin();
        let mut stdout = stdout();

        self.source_contents.insert(SESSION_SOURCE.into(), String::new());

        println!("Welcome to the vay! interactive session. Write `:help` for more information.");
        loop {
            print!("vay! > ");
            stdout.flush().unwrap();

            let mut input = String::new();
            // TODO: Error handling
            stdin.read_line(&mut input).unwrap();

            if input.trim_end().is_empty() {
                continue;
            }

            if input.starts_with(":") {
                let (command, input) = if input.contains(" ") {
                    let Some((command, input)) = input.split_once(" ") else {
                        panic!("No command provided.");
                    };

                    (&command[1..], input)
                } else {
                    (input[1..].trim_end(), "")
                };

                let Some(command) = COMMANDS.iter().find(|cmd| cmd.name == command) else {
                    error(&format!("unknown session command `{}`", command));
                    continue;
                };

                match (command.action)(self, input.into()) {
                    Ok(()) => (),
                    Err(error) => error.report(&self.source_contents[SESSION_SOURCE], "", &self.interner),
                };
            } else {
                *self.source_contents.get_mut(SESSION_SOURCE).unwrap() = input;

                let input = &self.source_contents[SESSION_SOURCE];
                let lexer = Lexer::new(SESSION_SOURCE.into(), input, &mut self.interner);
                let mut parser = Parser::new(lexer);

                let mut expression = runner_interactive!(self, parser.session_expression(), "parsing");
                runner_interactive!(self, self.resolver.expression(&mut expression), "name resolution");
                let t = runner_interactive!(self, self.checker.infer(&expression), "type checking");

                let ControlFlow::Ok(result) = self.interpreter.expression(&expression) else {
                    unreachable!()
                };
                println!("{} : {}", result.as_string(&self.interner), t.display(&self.interner))
            }

        }
    }
}

struct Command {
    name: &'static str,
    description: &'static str,
    action: fn(runner: &mut Runner, input: String) -> ReportableResult<()>
}

const COMMANDS: &[Command] = &[
    Command {
        name: "help",
        description: "Displays this message",
        action: |_, _| {
            help();
            Ok(())
        }
    },
    Command {
        name: "exit",
        description: "Exits the session",
        action: |_, _| exit(0)
    },
];

fn help() {
    println!();
    println!("    Session Commands:");
    for command in COMMANDS {
        println!("        {:<15} {}", command.name, command.description);
    }
}

fn error(msg: &str) {
    println!();
    println!("    Error: {}", msg);
    help();
}