use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::{ResolvedStruct, TypeId};
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::merger::type_collector::{collect_type_data, CollectedTypeData};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{
    ParsedModule, ParsedProgram, ParsedStructDefinition,
};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::{HashMap, HashSet};

pub fn build_resolved_types(program: &ParsedProgram) -> MergerResult<ResolvedTypes> {
    let collected_type_data = collect_type_data(program)?;
    let struct_defs = extract_module_struct_types(&program.module_tree)?;

    let mut resolved_structs = HashMap::new();

    for (module_id, module_struct_defs) in &struct_defs {
        for (_, struct_def) in module_struct_defs {
            let resolved_struct =
                resolve_struct_definition(struct_def, &collected_type_data, module_id)?;
            resolved_structs.insert(resolved_struct.id.clone(), resolved_struct);
        }
    }

    let type_sizes = compute_type_sizes(&resolved_structs, &collected_type_data);

    compute_struct_field_offsets(&mut resolved_structs, &type_sizes);

    Ok(ResolvedTypes {
        structs: resolved_structs,
        type_sizes,
        collected_type_data,
    })
}

fn compute_struct_field_offsets(
    resolved_structs: &mut HashMap<ItemId, ResolvedStruct>,
    type_sizes: &HashMap<TypeId, usize>,
) {
    for (_, resolved_struct) in resolved_structs {
        let mut offsets = HashMap::new();
        let mut offset = 0;
        for field_name in resolved_struct.field_order.iter().rev() {
            let field_type = resolved_struct.field_types.get(field_name).unwrap();
            offsets.insert(field_name.clone(), offset);
            offset += get_type_size(field_type, type_sizes);
        }
        resolved_struct.field_offsets = offsets;
    }
}

fn resolve_struct_definition(
    struct_def: &Src<ParsedStructDefinition>,
    collected_type_data: &CollectedTypeData,
    module_path: &ModuleIdentifier,
) -> MergerResult<ResolvedStruct> {
    let mut field_types = HashMap::new();
    let mut field_order = Vec::new();

    for (field_name, field_type) in &struct_def.value.fields {
        field_order.push(field_name.clone());
        field_types.insert(
            field_name.clone(),
            collected_type_data
                .map_parsed_type(field_type)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "Failed to resolve type for field '{}' in struct '{}' at {}",
                        field_name,
                        struct_def.value.struct_name,
                        struct_def.location
                    )
                })?,
        );
    }

    Ok(ResolvedStruct {
        id: ItemId {
            module_id: module_path.clone(),
            item_name: struct_def.value.struct_name.clone(),
        },
        field_types,
        field_order,
        field_offsets: HashMap::new(),
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

fn compute_type_sizes(
    resolved_structs: &HashMap<ItemId, ResolvedStruct>,
    collected_type_data: &CollectedTypeData,
) -> HashMap<TypeId, usize> {
    let mut type_sizes = HashMap::new();

    for (_, module_structs) in &collected_type_data.structs {
        for (_, structs) in module_structs {
            let mut visited = HashSet::new();
            let size = compute_type_size(&structs, resolved_structs, &mut visited).unwrap();
            type_sizes.insert(structs.clone(), size);
        }
    }

    type_sizes
}

fn compute_type_size(
    type_id: &TypeId,
    resolved_structs: &HashMap<ItemId, ResolvedStruct>,
    visited: &mut HashSet<TypeId>,
) -> MergerResult<usize> {
    match type_id {
        TypeId::StructType(id) => {
            if !visited.insert(type_id.clone()) {
                return Err(anyhow::anyhow!("Type cycle detected: {:?}", type_id));
            }
            let resolved_struct = resolved_structs.get(&id).unwrap();
            let mut size = 0;
            for field_name in &resolved_struct.field_order {
                let field_type = resolved_struct.field_types.get(field_name).unwrap();
                size += compute_type_size(field_type, resolved_structs, visited)?;
            }
            Ok(size)
        }
        TypeId::Pointer(_) => Ok(8),
        TypeId::Unit => Ok(0),
        TypeId::Bool => Ok(1),
        TypeId::Char => Ok(1),
        TypeId::Integer(size) => Ok(*size),
    }
}

fn get_type_size(type_id: &TypeId, type_sizes: &HashMap<TypeId, usize>) -> usize {
    match type_id {
        TypeId::StructType(_) => *type_sizes.get(&type_id).unwrap(),
        TypeId::Pointer(_) => 8,
        TypeId::Unit => 0,
        TypeId::Bool => 1,
        TypeId::Char => 1,
        TypeId::Integer(size) => *size,
    }
}
