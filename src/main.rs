use crate::{interner::Interner, lexer::Lexer};

mod interner;
mod lexer;
mod token;

fn main() {
    let source = "  Hello; World  ";
    let lexer = Lexer::new(source, Interner::new());

    for result in lexer {
        match result {
            Ok(token) => println!("{token}"),
            Err(error) => {
                println!("{error}");
                break;
            }
        }
    }
}
