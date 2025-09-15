use crate::lex::location::SourceLocation;

pub trait Reportable {
    fn location(&self) -> SourceLocation;
    fn source(&self) -> &str;
    fn description(&self) -> String;

    fn report(&self, source_content: &str, stage: &str) {
        let mut lines = source_content.lines();
        let location = self.location();
        let source = self.source();

        if location.is_absence() {
            eprintln!("\r");
            eprintln!("        | [{source}] (at {stage})\r");
            eprintln!("        |\r");
            eprintln!("        | {}\r", self.description());
            return;
        }

        let first_line_number = location.start().row();

        eprintln!("\r");
        eprintln!("        | [{source}:{first_line_number}:{}] (at {stage})\r", location.start().column());
        eprintln!("        |\r");

        if location.is_on_one_line() {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}\r");
            eprintln!("        | {:spaces$}{:^^carrots$}\r", "", "",
                spaces = (1..location.start().column()).len(),
                carrots = (location.start().column()..location.end().column()).len()
            );
            eprintln!("        | {}\r", self.description())
        } else {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}\r");
            eprintln!("        | {:spaces$}{:^^carrots$}\r", "", "",
                spaces = (1..location.start().column()).len(),
                carrots = (location.start().column()..first_line.chars().count() + 1).len()
            );

            for line_number in (first_line_number + 1)..location.end().row() {
                let line = lines.next().unwrap();
                eprintln!("  {line_number:>5} | {line}\r");
                eprintln!("        | {:^^carrots$}\r", "",
                    carrots = line.chars().count()
                )
            }

            let last_line = lines.next().unwrap();
            eprintln!("  {:>5} | {last_line}\r", location.end().row());
            eprintln!("        | {:^^carrots$}\r", "",
                carrots = (1..location.end().column()).len()
            );
            eprintln!("        | {}", self.description())
        }
    }
}

pub type ReportableResult<T> = Result<T, Box<dyn Reportable>>;
