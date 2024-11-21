use crate::compiler::lexer::{HasLocation, Location};
use std::fmt::{Debug, Display};

pub type ParseResult<T> = Result<T, LocationError>;

pub struct LocationError {
    pub message: String,
    pub location: Location,
}

impl LocationError {
    pub fn msg(msg: &str, location: &Location) -> Self {
        LocationError {
            message: msg.to_string(),
            location: location.clone(),
        }
    }
    pub fn expect<T: Debug, T2: Debug + HasLocation>(expected: T, actual: &T2) -> Self {
        LocationError {
            message: format!("Expected {:?}, found {:?}", expected, actual),
            location: actual.location().clone(),
        }
    }
}

impl Display for LocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Parser error at line {}, column {}: {}",
            self.location.line, self.location.column, self.message
        )
    }
}

impl Debug for LocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
