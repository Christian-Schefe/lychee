use crate::compiler::merger::function_resolver::build_resolved_functions;
use crate::compiler::merger::merged_expression::MergedProgram;
use crate::compiler::merger::type_resolver::build_resolved_types;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::ParsedProgram;

pub fn merge_program(parsed_program: &ParsedProgram) -> MergerResult<MergedProgram> {
    let resolved_types = build_resolved_types(parsed_program)?;
    let resolved_functions = build_resolved_functions(parsed_program, &resolved_types)?;

    Ok(MergedProgram {
        resolved_functions,
        resolved_types,
        root_name: parsed_program.root_name.clone(),
    })
}
