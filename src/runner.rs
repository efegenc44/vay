use core::panic;

use std::{
    collections::HashMap,
    fs,
    io::{stdout, Write},
    process::exit,
};

use crate::{
    vay::{
        intrinsics::INTRINSICS_FILE_PATH,
        stdlib
    },
    check::Checker,
    vay::core::CORE_FILE_PATH,
    ast::{declaration::Module, parser::Parser},
    interpret::interpreter::{ControlFlow, Interpreter},
    lex::lexer::Lexer,
    reportable::ReportableResult,
    resolution::Resolver
};

use crossterm::{
    cursor::MoveTo,
    event::{Event, KeyCode, KeyModifiers},
    terminal,
    ExecutableCommand
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
                );
                continue
            }
        }
    };
}

pub struct Runner {
    source_contents: HashMap<String, String>,
    resolver: Resolver,
    checker: Checker,
    interpreter: Interpreter
}

impl Runner {
    pub fn new() -> Self {
        Self {
            source_contents: HashMap::new(),
            resolver: Resolver::new(),
            checker: Checker::new(),
            interpreter: Interpreter::new(),
        }
    }

    pub fn parse_all(&mut self, mut sources: Vec<String>) -> Result<Vec<Module>, ()> {
        sources.push(INTRINSICS_FILE_PATH.into());
        sources.push(CORE_FILE_PATH.into());
        sources.extend(stdlib::collect_std_file_paths());

        let mut modules = vec![];
        for source in sources {
            // TODO: Better error reporting here
            let source_content = fs::read_to_string(&source)
                .map_err(|error| println!("\n    Error {}: {}", source, error))?;
            self.source_contents.insert(source.clone(), source_content);
            let source_content = &self.source_contents[&source];
            print!("{:>20}: {}", "Parsing", source);
            let lexer = Lexer::new(source, source_content);
            let mut parser = Parser::new(lexer);
            let module = runner_handle!(self, parser.module(), "parsing");
            modules.push(module);
        }

        Ok(modules)
    }

    pub fn check(&mut self, modules: Vec<Module>) -> Result<Vec<Module>, ()> {
        print!("{:>20}:", "Name Resolution");
        let modules = runner_handle!(self, self.resolver.resolve(modules), "name resolution");
        print!("{:>20}:", "Type Checking");
        runner_handle!(self, self.checker.type_check(&modules), "type checking");
        Ok(modules)
    }

    pub fn interpret(&mut self, modules: &[Module]) {
        let mut interpreter = Interpreter::new();
        println!("{:>20}", "Interpreting");
        interpreter.evaluate_main(modules);
    }

    pub fn interactive(&mut self, session_start_module_paths: Vec<String>) {
        let modules = self.parse_all(session_start_module_paths).unwrap();
        let modules = self.check(modules).unwrap();

        for module in modules {
            self.interpreter.collect_names(&module);
        }

        self.resolver.init_interactive_module();
        self.checker.init_interactive_session();

        let mut stdout = stdout();

        self.source_contents.insert(SESSION_SOURCE.into(), String::new());

        let mut cursor_position;

        crossterm::terminal::enable_raw_mode().unwrap();

        let mut history: Vec<String> = vec![];
        let mut history_position = 0;

        write!(stdout, "Welcome to the vay! interactive session. Write `:help` for more information.\r\n").unwrap();
        loop {
            write!(stdout, "vay! > ").unwrap();
            stdout.flush().unwrap();
            let input_start = crossterm::cursor::position().unwrap();
            cursor_position = 0;

            let mut new_input = String::new();
            let mut input;

            loop {
                let history_len = history.len();
                input = if history_position == 0 {
                    &mut new_input
                } else {
                    &mut history[history_len - history_position]
                };

                let event = crossterm::event::read().unwrap();

                if let Event::Key(key) = event {
                    if let Some(ch) = key.code.as_char() {
                        if key.modifiers.contains(KeyModifiers::SHIFT) {
                            let uppercase = ch.to_uppercase().collect::<Vec<_>>();
                            for (i, ch) in uppercase.into_iter().enumerate() {
                                input.insert(cursor_position as usize + i, ch);
                            }
                        } else {
                            input.insert(cursor_position as usize, ch);
                        }
                        cursor_position += 1;
                    }
                }

                if event == Event::Key(KeyCode::Enter.into()) {
                    break;
                }

                if event == Event::Key(KeyCode::Backspace.into()) {
                    if cursor_position > 0 {
                        input.remove(cursor_position as usize - 1);
                        cursor_position -= 1;
                    }
                }

                if event == Event::Key(KeyCode::Right.into()) {
                    if cursor_position as usize + 1 <= input.len() {
                        cursor_position += 1;
                    }
                }

                if event == Event::Key(KeyCode::Left.into()) {
                    if cursor_position > 0 {
                        cursor_position -= 1;
                    }
                }

                if event == Event::Key(KeyCode::Up.into()) {
                    if history_position + 1 <= history_len {
                        history_position += 1;
                    }
                }

                if event == Event::Key(KeyCode::Down.into()) {
                    if history_position > 0 {
                        history_position -= 1;
                    }
                }

                stdout.execute(MoveTo(input_start.0, input_start.1)).unwrap();
                if history_position == 0 {
                    // FIXME: hack
                    write!(stdout, "{}                  ", new_input).unwrap();
                } else {
                    // FIXME: hack
                    write!(stdout, "{}                  ", history[history_len - history_position]).unwrap();
                }
                stdout.flush().unwrap();

                let input_len = if history_position == 0 {
                    new_input.len()
                } else {
                    history[history_len - history_position].len()
                };

                let pos_x = if cursor_position as usize > input_len {
                    cursor_position = input_len as u16;
                    input_len as u16 + input_start.0
                } else {
                    cursor_position + input_start.0
                };

                stdout.execute(MoveTo(pos_x, input_start.1)).unwrap();
            }
            write!(stdout, "\r\n").unwrap();
            let input = input.clone();

            if input.is_empty() {
                continue;
            }

            if let Some(input) = input.strip_prefix(":") {
                let (command, input) = if input.contains(" ") {
                    let Some((command, input)) = input.split_once(" ") else {
                        panic!("No command provided.");
                    };

                    (command, input)
                } else {
                    (input, "")
                };

                let Some(command) = COMMANDS.iter().find(|cmd| cmd.name == command) else {
                    error(&format!("unknown session command `{}`", command));
                    continue;
                };

                match (command.action)(self, input.into()) {
                    Ok(()) => (),
                    Err(error) => error.report(&self.source_contents[SESSION_SOURCE], "")
                };
            } else {
                *self.source_contents.get_mut(SESSION_SOURCE).unwrap() = input.clone();

                let input = &self.source_contents[SESSION_SOURCE];
                let lexer = Lexer::new(SESSION_SOURCE.into(), input);
                let mut parser = Parser::new(lexer);

                let mut expression = runner_interactive!(self, parser.session_expression(), "parsing");
                runner_interactive!(self, self.resolver.expression(&mut expression), "name resolution");
                let t = runner_interactive!(self, self.checker.infer(&expression), "type checking");

                let ControlFlow::Ok(result) = self.interpreter.expression(&expression) else {
                    unreachable!()
                };
                write!(stdout, "{} : {}\r\n", result, t).unwrap()
            }

            history.push(input);
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
        action: |_, _| {
            terminal::disable_raw_mode().unwrap();
            exit(0);
        }
    },
];

fn help() {
    println!("\r");
    println!("    Session Commands:\r");
    for command in COMMANDS {
        println!("        {:<15} {}\r", command.name, command.description);
    }
}

fn error(msg: &str) {
    println!("\r");
    println!("    Error: {}\r", msg);
    help();
}