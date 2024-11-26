use anyhow::Error;

mod type_resolver;
mod analyzed_expression;
pub mod program_analyzer;
mod expression_analyzer;

pub type AnalyzerResult<T> = Result<T, Error>;