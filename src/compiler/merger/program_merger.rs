use crate::compiler::merger::function_resolver::build_resolved_functions;
use crate::compiler::merger::merged_expression::{FunctionId, MergedProgram};
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::type_resolver::build_resolved_types;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{ParsedExpression, ParsedModule, ParsedProgram};

pub fn merge_program(parsed_program: &ParsedProgram) -> MergerResult<MergedProgram> {
    let resolved_types = build_resolved_types(parsed_program)?;
    let resolved_functions = build_resolved_functions(parsed_program, &resolved_types)?;

    let mut function_bodies = Vec::new();

    for (_, parsed_module) in &parsed_program.module_tree {
        merge_module(&mut function_bodies, &resolved_types, parsed_module)?;
    }

    Ok(MergedProgram {
        function_bodies,
        resolved_functions,
        resolved_types,
    })
}

pub fn merge_module(
    functions: &mut Vec<(FunctionId, ParsedExpression)>,
    resolved_types: &ResolvedTypes,
    parsed_module: &ParsedModule,
) -> MergerResult<()> {
    for function in &parsed_module.functions {
        let item_id = ItemId {
            item_name: function.value.function_name.clone(),
            module_id: parsed_module.module_path.clone(),
        };
        let func_id = FunctionId {
            item_id,
            impl_type: None,
        };
        functions.push((func_id, function.value.body.clone()));
    }

    for type_impl in &parsed_module.type_implementations {
        let resolved_type = resolved_types
            .resolve_generic_type(
                &type_impl.value.impl_type.value,
                &type_impl.value.generic_params,
            )
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Type {} not found at {}",
                    type_impl.value.impl_type.value,
                    type_impl.location
                )
            })?;
        for function in &type_impl.value.functions {
            let item_id = ItemId {
                item_name: function.value.function_name.clone(),
                module_id: parsed_module.module_path.clone(),
            };
            let func_id = FunctionId {
                item_id,
                impl_type: Some(resolved_type.clone()),
            };
            functions.push((func_id, function.value.body.clone()));
        }
    }

    Ok(())
}
