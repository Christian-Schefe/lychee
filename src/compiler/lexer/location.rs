use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Src<T> {
    pub value: T,
    pub location: Location,
}

impl<T> Src<T> {
    pub fn new(value: T, location: Location) -> Self {
        Self {
            value,
            location,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

impl Location {
    pub fn new() -> Self {
        Location { line: 1, column: 1 }
    }

    pub fn advance_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn advance_column(&mut self, offset: usize) {
        self.column += offset;
    }
}