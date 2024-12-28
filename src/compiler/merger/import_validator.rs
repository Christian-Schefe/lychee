use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::ParsedProgram;

pub fn validate_imports(
    program: &ParsedProgram,
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
) -> MergerResult<()> {
    for (_, module) in &program.module_tree {
        for import in &module.imports {
            let module_id = &import.value.module_id;
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(module_structs) =
                        resolved_types.collected_type_data.structs.get(module_id)
                    {
                        if module_structs.contains_key(obj) {
                            continue;
                        }
                    }
                    if let Some(module_enums) =
                        resolved_types.collected_type_data.enums.get(module_id)
                    {
                        if module_enums.contains_key(obj) {
                            continue;
                        }
                    }
                    if let Some(module_type_aliases) = resolved_types
                        .collected_type_data
                        .type_aliases
                        .get(module_id)
                    {
                        if module_type_aliases.contains_key(obj) {
                            continue;
                        }
                    }
                    if let Some(module_functions) = resolved_functions
                        .collected_function_data
                        .functions
                        .get(module_id)
                    {
                        if module_functions.contains_key(obj) {
                            continue;
                        }
                    }
                    Err(anyhow::anyhow!(
                        "Imported object not found: {} in module {}",
                        obj,
                        module_id.get_identifier()
                    ))?;
                }
            } else {
                if resolved_types
                    .collected_type_data
                    .structs
                    .contains_key(module_id)
                {
                    continue;
                }
                if resolved_types
                    .collected_type_data
                    .enums
                    .contains_key(module_id)
                {
                    continue;
                }
                if resolved_types
                    .collected_type_data
                    .type_aliases
                    .contains_key(module_id)
                {
                    continue;
                }
                if resolved_functions
                    .collected_function_data
                    .functions
                    .contains_key(module_id)
                {
                    continue;
                }
                Err(anyhow::anyhow!(
                    "Imported module not found: {}",
                    module_id.get_identifier()
                ))?;
            }
        }
    }

    Ok(())
}
