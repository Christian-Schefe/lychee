use anyhow::Error;

pub mod analyzed_expression;
mod expression_analyzer;
pub mod program_analyzer;
mod return_analyzer;
pub mod type_resolver;

pub type AnalyzerResult<T> = Result<T, Error>;
