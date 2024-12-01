use crate::compiler::lexer::Location;
use anyhow::Error;
use std::fmt::Display;
use thiserror::Error;

pub type LexResult<T> = Result<T, Error>;

#[derive(Clone, Debug, Error)]
pub struct LocationError {
    pub message: String,
    pub location: Location,
}

impl LocationError {
    pub fn new(message: String, location: Location) -> Self {
        LocationError { message, location }
    }
}

impl Display for LocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} at {}.", self.message, self.location)
    }
}
