use anyhow::Error;

pub mod analyzed_expression;
pub mod analyzed_type;
mod expression_analyzer;
pub mod iterative_expression_analyzer;
pub mod program_analyzer;
mod return_analyzer;
pub mod analyzed_expression_printer;

pub type AnalyzerResult<T> = Result<T, Error>;
