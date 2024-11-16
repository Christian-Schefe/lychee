use std::fmt::{Debug, Display};
use crate::lexer::{HasLocation, Location, SrcToken};

pub type ParseResult<T> = Result<T, LocationError>;

pub struct LocationError {
    pub message: String,
    pub location: Location,
}

impl LocationError {
    pub fn expect<T: Debug, T2: Debug + HasLocation>(expected: T, actual: &T2) -> Self {
        LocationError {
            message: format!("Expected {:?}, found {:?}", expected, actual),
            location: actual.location().clone(),
        }
    }
    pub fn expect2<T: Debug, T2: Debug>(expected: T, actual1: &T2, actual2: &T2, location: &Location) -> Self {
        LocationError {
            message: format!("Expected {:?}, found {:?} and {:?}", expected, actual1, actual2),
            location: location.clone(),
        }
    }
    pub fn expect_at<T: Debug, T2: Debug>(expected: T, actual: T2, location: &Location) -> Self {
        LocationError {
            message: format!("Expected {:?}, found {:?}", expected, actual),
            location: location.clone(),
        }
    }
}

impl Display for LocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Parser error at line {}, column {}: {}", self.location.line, self.location.column, self.message)
    }
}

impl Debug for LocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}