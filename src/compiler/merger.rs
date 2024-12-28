use crate::compiler::merger::merged_expression::MergedProgram;
use crate::compiler::parser::parsed_expression::ParsedProgram;
use anyhow::Error;

mod function_collector;
mod function_resolver;
pub mod merged_expression;
mod program_merger;
pub mod resolved_functions;
pub mod resolved_types;
mod type_collector;
mod type_resolver;
mod import_validator;

pub type MergerResult<T> = Result<T, Error>;

pub fn merge_program(parsed_program: &ParsedProgram) -> MergedProgram {
    program_merger::merge_program(parsed_program).unwrap()
}
