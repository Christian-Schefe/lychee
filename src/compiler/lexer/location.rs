use crate::compiler::parser::ModulePath;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Src<T> {
    pub value: T,
    pub location: Location,
}

impl<T> Src<T> {
    pub fn new(value: T, location: Location) -> Self {
        Self { value, location }
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub file: Option<ModulePath>,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "line {}, column {} ({})",
            self.line,
            self.column,
            self.file
                .as_ref()
                .map(|x| format!("{}", x.file.display()))
                .unwrap_or("unknown".to_string())
        )
    }
}

impl Location {
    pub fn new(file: ModulePath) -> Self {
        Location {
            line: 1,
            column: 1,
            file: Some(file),
        }
    }

    pub fn default() -> Self {
        Location {
            line: 1,
            column: 1,
            file: None,
        }
    }

    pub fn advance_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn advance_column(&mut self, offset: usize) {
        self.column += offset;
    }
}
