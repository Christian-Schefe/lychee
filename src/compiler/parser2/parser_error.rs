use std::fmt::Display;
use anyhow::Error;

pub type ParseResult<T> = Result<T, Error>;
