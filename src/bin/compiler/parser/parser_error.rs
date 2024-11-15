use std::fmt::{Debug, Display};
use crate::lexer::{SrcToken, Token};

pub type ParseResult<T> = Result<T, ParserError>;

pub struct ParserError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl ParserError {
    pub fn expect<T: Debug>(expected: T, actual: &SrcToken) -> Self {
        ParserError {
            message: format!("Expected {:?}, found {:?}", expected, actual.token),
            line: actual.line,
            column: actual.column,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Parser error at line {}, column {}: {}", self.line, self.column, self.message)
    }
}

impl Debug for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}