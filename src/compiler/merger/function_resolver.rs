use crate::compiler::analyzer::analyzed_type::{GenericIdKind, GenericParams};
use crate::compiler::lexer::location::Src;
use crate::compiler::merger::function_collector::collect_function_data;
use crate::compiler::merger::merged_expression::{FunctionId, ResolvedFunctionHeader};
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{ParsedFunction, ParsedProgram};
use std::collections::HashMap;

pub fn build_resolved_functions(
    program: &ParsedProgram,
    resolved_types: &ResolvedTypes,
) -> MergerResult<ResolvedFunctions> {
    let (collected_function_data, function_bodies) = collect_function_data(program)?;
    let mut functions = HashMap::new();
    crate::compiler::builtin::BuiltinFunction::add_builtin_function_headers(&mut functions);

    extract_module_functions(resolved_types, &mut functions, &function_bodies)?;

    let mapped_bodies = function_bodies
        .into_iter()
        .map(|(id, body)| (id, body.value.body))
        .collect();

    Ok(ResolvedFunctions {
        function_headers: functions,
        collected_function_data,
        function_bodies: mapped_bodies,
    })
}

fn extract_module_functions(
    resolved_types: &ResolvedTypes,
    functions: &mut HashMap<FunctionId, ResolvedFunctionHeader>,
    function_bodies: &Vec<(FunctionId, Src<ParsedFunction>)>,
) -> MergerResult<()> {
    for (id, func_def) in function_bodies {
        let header = extract_function(func_def, resolved_types, id.clone())?;
        if functions.insert(id.clone(), header).is_some() {
            return Err(anyhow::anyhow!("Duplicate function definition: {}", id));
        }
    }
    Ok(())
}

fn extract_function(
    func_def: &Src<ParsedFunction>,
    resolved_types: &ResolvedTypes,
    func_id: FunctionId,
) -> MergerResult<ResolvedFunctionHeader> {
    let signature = &func_def.value.signature;
    let mut parameter_order = Vec::with_capacity(signature.params.len());
    let mut parameter_types = HashMap::with_capacity(signature.params.len());

    let generic_params = signature.generic_params.clone();
    let resolved_generic_params =
        GenericParams::from(GenericIdKind::Function(func_id.clone()), &generic_params);

    for (arg_type, arg_name) in &signature.params {
        parameter_order.push(arg_name.clone());
        let resolved_arg_type = resolved_types
            .map_generic_parsed_type(&arg_type.value, &resolved_generic_params)
            .ok_or_else(|| {
                anyhow::anyhow!("Type {} not found at {}", arg_type.value, func_def.location)
            })?;
        if parameter_types
            .insert(arg_name.clone(), resolved_arg_type)
            .is_some()
        {
            return Err(anyhow::anyhow!(
                "Duplicate parameter name: {}",
                arg_name.clone()
            ));
        }
    }

    let resolved_return_type = resolved_types
        .map_generic_parsed_type(&signature.return_type.value, &resolved_generic_params)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "Return type {} not found at {}",
                signature.return_type.value,
                func_def.location
            )
        })?;

    let header = ResolvedFunctionHeader {
        id: func_id,
        return_type: resolved_return_type,
        parameter_order,
        parameter_types,
        generic_params: resolved_generic_params,
    };

    Ok(header)
}
