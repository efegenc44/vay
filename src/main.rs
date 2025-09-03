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
mod commandline;
mod runner;
mod intrinsics;
mod core;
mod stdlib;
mod type_expression;

fn main() {
    commandline::execute()
}
