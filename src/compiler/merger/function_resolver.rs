use crate::compiler::analyzer::analyzed_type::{GenericId, GenericIdKind, GenericParams};
use crate::compiler::lexer::location::Src;
use crate::compiler::merger::function_collector::collect_function_data;
use crate::compiler::merger::merged_expression::{FunctionId, ResolvedFunctionHeader};
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{ParsedFunction, ParsedModule, ParsedProgram};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub fn build_resolved_functions(
    program: &ParsedProgram,
    resolved_types: &ResolvedTypes,
) -> MergerResult<ResolvedFunctions> {
    let collected_function_data = collect_function_data(program)?;
    let mut functions = HashMap::new();
    crate::compiler::builtin::BuiltinFunction::add_builtin_function_headers(&mut functions);

    extract_module_functions(resolved_types, &mut functions, &program.module_tree)?;

    Ok(ResolvedFunctions {
        function_headers: functions,
        collected_function_data,
    })
}

fn extract_module_functions(
    resolved_types: &ResolvedTypes,
    functions: &mut HashMap<FunctionId, ResolvedFunctionHeader>,
    module_tree: &HashMap<ModuleIdentifier, ParsedModule>,
) -> MergerResult<()> {
    for module in module_tree.values() {
        for func_def in &module.functions {
            let item_id = ItemId {
                item_name: func_def.value.function_name.clone(),
                module_id: module.module_path.clone(),
            };
            let func_id = FunctionId {
                id: item_id,
                param_count: func_def.value.params.len(),
                generic_count: func_def.value.generic_params.order.len(),
            };
            let header = extract_function(func_def, resolved_types, func_id.clone())?;

            if functions.insert(func_id.clone(), header).is_some() {
                return Err(anyhow::anyhow!(
                    "Duplicate function definition: {}",
                    func_id
                ));
            }
        }
    }
    Ok(())
}

fn extract_function(
    func_def: &Src<ParsedFunction>,
    resolved_types: &ResolvedTypes,
    func_id: FunctionId,
) -> MergerResult<ResolvedFunctionHeader> {
    let mut parameter_order = Vec::with_capacity(func_def.value.params.len());
    let mut parameter_types = HashMap::with_capacity(func_def.value.params.len());

    let generic_params = func_def.value.generic_params.clone();
    let resolved_generic_params =
        GenericParams::from(GenericIdKind::Function(func_id.clone()), &generic_params);

    for (arg_type, arg_name) in &func_def.value.params {
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
        .map_generic_parsed_type(&func_def.value.return_type.value, &resolved_generic_params)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "Return type {} not found at {}",
                func_def.value.return_type.value,
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
