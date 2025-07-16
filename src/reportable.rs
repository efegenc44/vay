use crate::{interner::Interner, location::SourceLocation};

pub trait Reportable {
    fn location(&self) -> SourceLocation;
    fn source(&self) -> &str;
    fn description(&self, interner: &Interner) -> String;

    fn report(&self, source_content: &str, stage: &str, interner: &Interner) {
        let mut lines = source_content.lines();
        let location = self.location();
        let source = self.source();

        if location.is_absence() {
            eprintln!();
            eprintln!("        | [{source}] (at {stage})");
            eprintln!("        |");
            eprintln!("        | {}", self.description(interner));
            return;
        }

        let first_line_number = location.start().row();

        eprintln!();
        eprintln!("        | [{source}:{first_line_number}:{}] (at {stage})", location.start().column());
        eprintln!("        |");

        if location.is_on_one_line() {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!("        | {:spaces$}{:^^carrots$}", "", "",
                spaces = (1..location.start().column()).len(),
                carrots = (location.start().column()..location.end().column()).len()
            );
            eprintln!("        | {}", self.description(interner))
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
                eprintln!("        | {:^^carrots$}", "",
                    carrots = line.chars().count()
                )
            }

            let last_line = lines.next().unwrap();
            eprintln!("  {:>5} | {last_line}", location.end().row());
            eprintln!("        | {:^^carrots$}", "",
                carrots = (1..location.end().column()).len()
            );
            eprintln!("        | {}", self.description(interner))
        }
    }
}

pub type ReportableResult<T> = Result<T, Box<dyn Reportable>>;
