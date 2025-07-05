use std::fmt::Display;

#[derive(Clone, Copy)]
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
}

#[derive(Clone, Copy)]
pub struct SourceLocation {
    start: Position,
    end: Position,
}

impl SourceLocation {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn extend(&self, location: &Self) -> Self {
        assert!(self.start.row <= location.start.row);
        assert!(self.end.row <= location.end.row);

        Self::new(self.start, location.end)
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

    pub fn move_data(self) -> T {
        self.data
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
