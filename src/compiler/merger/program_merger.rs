use crate::compiler::lexer::location::Src;
use crate::compiler::merger::function_resolver::build_resolved_functions;
use crate::compiler::merger::iterative_expression_merger::merge_expression;
use crate::compiler::merger::merged_expression::{
    MergedFunction, MergedProgram, ModuleId, ResolvedFunctions, ResolvedTypes,
};
use crate::compiler::merger::type_resolver::build_resolved_types;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{ParsedFunction, ParsedModule, ParsedProgram};
use crate::compiler::parser::ModulePath;
use std::collections::HashMap;

pub fn merge_program(parsed_program: &ParsedProgram) -> MergerResult<MergedProgram> {
    let resolved_types = build_resolved_types(parsed_program)?;
    let resolved_functions = build_resolved_functions(parsed_program, &resolved_types)?;

    let mut functions = HashMap::new();

    for (_, parsed_module) in &parsed_program.module_tree {
        merge_module(
            &mut functions,
            &resolved_types,
            &resolved_functions,
            parsed_module,
        )?;
    }

    Ok(MergedProgram {
        functions,
        resolved_functions,
        resolved_types,
    })
}

pub fn merge_module(
    functions: &mut HashMap<ModuleId, Src<MergedFunction>>,
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    parsed_module: &ParsedModule,
) -> MergerResult<()> {
    for function in &parsed_module.functions {
        let module_id = ModuleId {
            name: function.value.function_name.clone(),
            module_path: parsed_module.module_path.clone(),
        };
        let merged_function = merge_function(
            resolved_types,
            resolved_functions,
            &parsed_module.module_path,
            function,
        )?;
        functions.insert(
            module_id,
            Src {
                location: function.location.clone(),
                value: merged_function,
            },
        );
    }

    Ok(())
}

pub fn merge_function(
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    module_path: &ModulePath,
    parsed_function: &Src<ParsedFunction>,
) -> MergerResult<MergedFunction> {
    let body = merge_expression(
        resolved_types,
        resolved_functions,
        module_path,
        &parsed_function.value.body,
    )?;

    Ok(MergedFunction { body })
}
