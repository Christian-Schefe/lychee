use anyhow::Error;

pub mod type_resolver;
pub mod analyzed_expression;
pub mod program_analyzer;
mod expression_analyzer;
mod return_analyzer;

pub type AnalyzerResult<T> = Result<T, Error>;