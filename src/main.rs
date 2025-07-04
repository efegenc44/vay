use crate::lexer::Lexer;

mod lexer;
mod token;

fn main() {
    let source = "  Hello; World  ";
    let lexer = Lexer::new(source);

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
