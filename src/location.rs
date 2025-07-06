use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Position {
    row: usize,
    column: usize,
}

impl Position {
    pub fn new(row: usize, column: usize) -> Self {
        Self { row, column }
    }

    pub fn advance(&mut self) {
        self.column += 1;
    }

    pub fn newline(&mut self) {
        self.column = 1;
        self.row += 1;
    }

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
    start: Position,
    end: Position,
}

impl SourceLocation {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn dummy() -> Self {
        Self { start: Position::new(0, 0), end: Position::new(0, 0) }
    }

    pub fn extend(&self, location: &Self) -> Self {
        assert!(self.start.row <= location.start.row);
        assert!(self.end.row <= location.end.row);

        Self::new(self.start, location.end)
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }

    pub fn is_on_one_line(&self) -> bool {
        self.start().row() == self.end().row()
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} - {}:{}",
            self.start.row, self.start.column, self.end.row, self.end.column
        )
    }
}

#[derive(Clone, Copy)]
pub struct Located<T> {
    data: T,
    location: SourceLocation,
}

impl<T> Located<T> {
    pub fn new(data: T, location: SourceLocation) -> Self {
        Self { data, location }
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut T {
        &mut self.data
    }

    pub fn location(&self) -> SourceLocation {
        self.location
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.data, self.location)
    }
}
