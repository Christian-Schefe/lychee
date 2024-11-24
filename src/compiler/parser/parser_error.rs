use crate::compiler::lexer::{HasLocation, Location};
use std::fmt::{Debug, Display};
use anyhow::Error;
use thiserror::Error;

pub type ParseResult<T> = Result<T, Error>;
pub type MaybeParseResult<T> = Result<T, MaybeError>;

#[derive(Error, Debug)]
pub struct MaybeError {
    pub should_propagate: bool,
    pub error: Error,
}
pub fn transform_propagate<T>(result: ParseResult<T>, should_propagate: bool) -> MaybeParseResult<T> {
    match result {
        Ok(val) => Ok(val),
        Err(err) => Err(MaybeError::new(err, should_propagate)),
    }
}

pub fn add_context<T>(result: MaybeParseResult<T>, message: String) -> MaybeParseResult<T>
{
    match result {
        Ok(val) => Ok(val),
        Err(err) => Err(MaybeError::new(err.error.context(message), err.should_propagate)),
    }
}

pub fn consume_propagate<T>(result: MaybeParseResult<T>) -> MaybeParseResult<Option<T>> {
    match result {
        Ok(val) => Ok(Some(val)),
        Err(err) => if err.should_propagate {
            Err(err)
        } else {
            Ok(None)
        },
    }
}

impl MaybeError {
    pub fn new(error: Error, should_propagate: bool) -> Self {
        MaybeError {
            should_propagate,
            error,
        }
    }
}

impl Display for MaybeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.error)
    }
}

#[derive(Clone, Error)]
pub struct LocationError {
    pub message: String,
    pub location: Location,
}

impl LocationError {
    pub fn msg(msg: &str, location: &Location) -> Error {
        Error::from(LocationError {
            message: msg.to_string(),
            location: location.clone(),
        })
    }
    pub fn expect<T: Debug, T2: Debug + HasLocation>(expected: T, actual: &T2) -> Error {
        Error::from(LocationError {
            message: format!("Expected {:?}, found {:?}", expected, actual),
            location: actual.location().clone(),
        })
    }
}

impl Display for LocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "[ln. {}, col. {}] {}",
            self.location.line, self.location.column, self.message
        )
    }
}

impl Debug for LocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
