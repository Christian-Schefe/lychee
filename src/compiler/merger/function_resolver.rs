use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::builtin;
use crate::compiler::lexer::location::Src;
use crate::compiler::merger::function_collector::collect_function_data;
use crate::compiler::merger::merged_expression::ResolvedFunctionHeader;
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
    let collected_function_data = collect_function_data(program, resolved_types)?;
    let mut functions = HashMap::new();
    builtin::BuiltinFunction::add_builtin_function_headers(&mut functions);

    let mut member_functions = HashMap::new();

    extract_module_functions(
        resolved_types,
        &mut functions,
        &mut member_functions,
        &program.module_tree,
    )?;

    Ok(ResolvedFunctions {
        functions,
        member_functions,
        collected_function_data,
    })
}

fn extract_module_functions(
    resolved_types: &ResolvedTypes,
    functions: &mut HashMap<ItemId, ResolvedFunctionHeader>,
    member_functions: &mut HashMap<AnalyzedTypeId, HashMap<ItemId, ResolvedFunctionHeader>>,
    module_tree: &HashMap<ModuleIdentifier, ParsedModule>,
) -> MergerResult<()> {
    for module in module_tree.values() {
        for func_def in &module.functions {
            let func_id = ItemId {
                item_name: func_def.value.function_name.clone(),
                module_id: module.module_path.clone(),
            };
            let header = extract_function(func_def, resolved_types)?;

            if functions.insert(func_id.clone(), header).is_some() {
                return Err(anyhow::anyhow!(
                    "Duplicate function definition: {}",
                    func_id
                ));
            }
        }
        for type_impl in &module.type_implementations {
            let resolved_type = resolved_types
                .resolve_type(&type_impl.value.impl_type.value)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "Type {:?} not found at {}",
                        type_impl.value.impl_type.value,
                        type_impl.location
                    )
                })?;
            let impl_functions = member_functions
                .entry(resolved_type)
                .or_insert_with(HashMap::new);
            for func_def in &type_impl.value.functions {
                let header = extract_function(func_def, resolved_types)?;
                let id = ItemId {
                    item_name: func_def.value.function_name.clone(),
                    module_id: module.module_path.clone(),
                };

                if impl_functions.insert(id.clone(), header).is_some() {
                    return Err(anyhow::anyhow!(
                        "Duplicate member function definition: {}",
                        id
                    ));
                }
            }
        }
    }
    Ok(())
}

fn extract_function(
    func_def: &Src<ParsedFunction>,
    resolved_types: &ResolvedTypes,
) -> MergerResult<ResolvedFunctionHeader> {
    let mut parameter_order = Vec::with_capacity(func_def.value.args.len());
    let mut parameter_types = HashMap::with_capacity(func_def.value.args.len());

    for (arg_type, arg_name) in &func_def.value.args {
        parameter_order.push(arg_name.clone());
        let resolved_arg_type = resolved_types
            .resolve_generic_type(&arg_type.value, &func_def.value.generics)
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
        .resolve_generic_type(&func_def.value.return_type.value, &func_def.value.generics)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "Return type {} not found at {}",
                func_def.value.return_type.value,
                func_def.location
            )
        })?;

    let header = ResolvedFunctionHeader {
        return_type: resolved_return_type,
        parameter_order,
        parameter_types,
        generic_params: func_def.value.generics.clone(),
    };

    Ok(header)
}
