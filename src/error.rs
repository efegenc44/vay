use crate::location::SourceLocation;

pub trait Error {
    fn location(&self) -> SourceLocation;
    fn description(&self) -> String;

    fn print(&self, source_name: &str, source: &str, stage: &str) {
        let mut lines = source.lines();
        let location = self.location();

        if location == SourceLocation::dummy() {
            eprintln!();
            eprintln!("  Error | [{source_name}] (at {stage})");
            eprintln!("        |");
            eprintln!("        | {}", self.description());
            return;
        }

        let first_line_number = location.start().row();

        eprintln!();
        eprintln!("  Error | [{source_name}:{first_line_number}:{}] (at {stage})", location.start().column());
        eprintln!("        |");

        if location.is_on_one_line() {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!("        | {:spaces$}{:^^carrots$}", "", "",
                spaces = (1..location.start().column()).len(),
                carrots = (location.start().column()..location.end().column()).len()
            );
            eprintln!("        | {}", self.description())
        } else {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!("        | {:spaces$}{:^^carrots$}", "", "",
                spaces = (1..location.start().column()).len(),
                carrots = (location.start().column()..first_line.chars().count()).len()
            );

            for line_number in (first_line_number + 1)..location.end().row() {
                let line = lines.next().unwrap();
                eprintln!("  {line_number:>5} | {line}");
                eprintln!("        | {:^^carrots$}", "", carrots = line.chars().count())
            }

            let last_line = lines.next().unwrap();
            eprintln!("  {:>5} | {last_line}", location.end().row());
            eprintln!("        | {:^^carrots$}", "",
                carrots = (1..location.end().column()).len()
            );
            eprintln!("        | {}", self.description())
        }
    }
}