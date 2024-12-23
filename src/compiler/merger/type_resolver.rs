use crate::compiler::analyzer::analyzed_type::{GenericIdKind, GenericParams};
use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::{ResolvedStruct, StructId};
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::type_collector::{collect_type_data, CollectedTypeData};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{
    ParsedModule, ParsedProgram, ParsedStructDefinition,
};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub fn build_resolved_types(program: &ParsedProgram) -> MergerResult<ResolvedTypes> {
    let collected_type_data = collect_type_data(program)?;
    println!("{:?}", collected_type_data);

    let struct_defs = extract_module_struct_types(&program.module_tree)?;

    let mut resolved_structs = HashMap::new();

    for (module_id, module_struct_defs) in &struct_defs {
        for (_, struct_def) in module_struct_defs {
            let resolved_struct =
                resolve_struct_definition(struct_def, module_id, &collected_type_data)?;
            resolved_structs.insert(resolved_struct.id.clone(), resolved_struct);
        }
    }

    Ok(ResolvedTypes {
        structs: resolved_structs,
        collected_type_data,
    })
}

fn resolve_struct_definition(
    struct_def: &Src<ParsedStructDefinition>,
    module_path: &ModuleIdentifier,
    collected_type_data: &CollectedTypeData,
) -> MergerResult<ResolvedStruct> {
    let struct_id = StructId {
        id: ItemId {
            module_id: module_path.clone(),
            item_name: struct_def.value.struct_name.clone(),
        },
        generic_count: struct_def.value.generics.order.len(),
    };

    let mut field_types = HashMap::new();
    let mut field_order = Vec::new();

    let resolved_generic_params = GenericParams::from(
        GenericIdKind::Struct(struct_id.clone()),
        &struct_def.value.generics,
    );

    for (field_name, field_type) in &struct_def.value.fields {
        field_order.push(field_name.clone());
        let resolved_field_type = collected_type_data
            .map_generic_parsed_type(&field_type.value, &resolved_generic_params)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Type {} not found at {}",
                    field_type.value,
                    field_type.location
                )
            })?;
        if field_types
            .insert(field_name.clone(), resolved_field_type)
            .is_some()
        {
            return Err(anyhow::anyhow!(
                "Duplicate field name '{}' in struct '{}' at {}",
                field_name,
                struct_def.value.struct_name,
                struct_def.location
            ));
        }
    }

    Ok(ResolvedStruct {
        id: struct_id,
        field_types,
        field_order,
        generic_params: resolved_generic_params,
    })
}

fn extract_module_struct_types(
    module_tree: &HashMap<ModuleIdentifier, ParsedModule>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, Src<ParsedStructDefinition>>>> {
    let mut struct_defs = HashMap::new();
    for module in module_tree.values() {
        let mut module_structs = HashMap::new();
        for struct_def in &module.struct_definitions {
            if let Some(_) =
                module_structs.insert(struct_def.value.struct_name.clone(), struct_def.clone())
            {
                return Err(anyhow::anyhow!(
                    "Duplicate struct definition '{}' at {}",
                    struct_def.value.struct_name.clone(),
                    struct_def.location
                ));
            }
        }
        struct_defs.insert(module.module_path.clone(), module_structs);
    }
    Ok(struct_defs)
}
