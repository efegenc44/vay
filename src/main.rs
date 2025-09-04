mod interner;
mod reportable;
mod runner;
mod commandline;
mod ast;
mod lex;
mod name;
mod check;
mod vay;
mod interpret;

fn main() {
    commandline::execute()
}
