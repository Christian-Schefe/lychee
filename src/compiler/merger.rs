use crate::compiler::merger::merged_expression::MergedProgram;
use crate::compiler::parser::parsed_expression::ParsedProgram;
use anyhow::Error;

mod expression_merger;
pub mod merged_expression;
mod program_merger;
mod type_resolver;
mod function_resolver;

pub type MergerResult<T> = Result<T, Error>;

pub fn merge_program(parsed_program: &ParsedProgram) -> MergedProgram {
    program_merger::merge_program(parsed_program).unwrap()
}
