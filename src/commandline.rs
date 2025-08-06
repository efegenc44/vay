use std::env;

use crate::runner::Runner;

struct Command {
    name: &'static str,
    arguments: &'static [&'static str],
    description: &'static str,
    action: fn(arguments: Vec<String>) -> Result<(), ()>
}

const COMMANDS: &[Command] = &[
    Command {
        name: "check",
        arguments: &["FILE*"],
        description: "Check for errors in a vay! program",
        action: |arguments| {
            let mut runner = Runner::new();
            let modules = runner.parse_all(arguments)?;
            let _  = runner.check(modules)?;
            Ok(())
        },
    },
    Command {
        name: "interpret",
        arguments: &["FILE*"],
        description: "Interpret a vay! program",
        action: |arguments| {
            let mut runner = Runner::new();
            let modules = runner.parse_all(arguments)?;
            let modules = runner.check(modules)?;
            runner.interpret(&modules);
            Ok(())
        },
    },
    Command {
        name: "interactive",
        arguments: &["FILE*"],
        description: "Enter the interactive session for vay! with the files provided",
        action: |arguments| {
            let mut runner = Runner::new();
            runner.interactive(arguments);
            Ok(())
        },
    },
    Command {
        name: "help",
        arguments: &["[COMMAND]"],
        description: "Display this message",
        action: |arguments| {
            if !arguments.is_empty() {
                let command = arguments.first().unwrap();
                let Some(command) = COMMANDS.iter().find(|cmd| cmd.name == command) else {
                    error(&format!("unknown command `{}`", command));
                    return Err(());
                };

                help_command(command);
            } else {
                help();
            }

            Ok(())
        },
    }
];

fn help() {
    println!();
    println!("    Usage: vay COMMAND [ARGUMENT*]");
    println!();
    println!("    Commands:");
    for command in COMMANDS {
        println!("        {:<15} {}", command.name, command.description);
    }
}

fn help_command(command: &Command) {
    println!();
    println!("    Usage: vay {} {}", command.name, command.arguments.join(","));
    println!();
    println!("        {}", command.description);
}

fn error(msg: &str) {
    println!();
    println!("    Error: {}", msg);
    help();
}

pub fn execute() {
    let mut arguments = env::args().skip(1);
    let Some(command) = arguments.next() else {
        return error("no command given.");
    };

    let Some(command) = COMMANDS.iter().find(|cmd| cmd.name == command) else {
        return error(&format!("unknown command `{}`", command));
    };

    let _ = (command.action)(arguments.collect());
}