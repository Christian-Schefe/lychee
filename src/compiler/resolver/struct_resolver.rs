use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::analyzer::iterative_expression_analyzer::resolve_generic_type;
use crate::compiler::analyzer::program_analyzer::GenericInstances;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::merger::merged_expression::ResolvedStruct;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::GenericParams;
use std::collections::{HashMap, HashSet};

pub struct ResolvedStructs {
    pub struct_sizes: HashMap<AnalyzedTypeId, usize>,
    pub field_offsets: HashMap<AnalyzedTypeId, HashMap<String, usize>>,
}

pub fn resolve_structs(
    resolved_types: &ResolvedTypes,
    generic_instances: &GenericInstances,
) -> ResolvedStructs {
    let mut struct_sizes = HashMap::new();
    compute_type_sizes(
        generic_instances,
        &resolved_types.structs,
        &mut struct_sizes,
    );

    let mut field_offsets = HashMap::new();
    for (id, resolved_struct) in &resolved_types.structs {
        if resolved_struct.generic_params.is_some() {
            for instance in &generic_instances.types[id] {
                let type_id = AnalyzedTypeId::StructType(id.clone(), instance.clone());
                let mut offsets = HashMap::new();
                let mut offset = 0;
                for field_name in resolved_struct.field_order.iter().rev() {
                    let field_type = resolved_struct.field_types.get(field_name).unwrap();
                    let actual_field_type =
                        resolve_generic_type(field_type, &resolved_struct.generic_params, instance);
                    offsets.insert(field_name.clone(), offset);
                    offset += get_type_size(&actual_field_type, &struct_sizes);
                }
                field_offsets.insert(type_id.clone(), offsets);
            }
        } else {
            let mut offsets = HashMap::new();
            let mut offset = 0;
            for field_name in resolved_struct.field_order.iter().rev() {
                let field_type = resolved_struct.field_types.get(field_name).unwrap();
                offsets.insert(field_name.clone(), offset);
                offset += get_type_size(field_type, &struct_sizes);
            }
            let type_id = AnalyzedTypeId::StructType(id.clone(), Vec::new());
            field_offsets.insert(type_id.clone(), offsets);
        }
    }

    ResolvedStructs {
        struct_sizes,
        field_offsets,
    }
}

fn compute_type_sizes(
    generic_instances: &GenericInstances,
    resolved_structs: &HashMap<ItemId, ResolvedStruct>,
    struct_sizes: &mut HashMap<AnalyzedTypeId, usize>,
) {
    for (id, resolved_struct) in resolved_structs {
        if resolved_struct.generic_params.is_some() {
            for instance in &generic_instances.types[id] {
                let type_id = AnalyzedTypeId::StructType(id.clone(), instance.clone());
                let mut visited = HashSet::new();
                let size = compute_type_size(
                    &type_id,
                    resolved_structs,
                    &mut visited,
                    &resolved_struct.generic_params,
                    instance,
                )
                .unwrap();
                struct_sizes.insert(type_id, size);
            }
        } else {
            let mut visited = HashSet::new();
            let type_id = AnalyzedTypeId::StructType(id.clone(), Vec::new());
            let size =
                compute_type_size(&type_id, resolved_structs, &mut visited, &None, &Vec::new())
                    .unwrap();
            struct_sizes.insert(type_id, size);
        }
    }
}

fn compute_type_size(
    type_id: &AnalyzedTypeId,
    resolved_structs: &HashMap<ItemId, ResolvedStruct>,
    visited: &mut HashSet<ItemId>,
    generic_params: &Option<GenericParams>,
    generic_args: &Vec<AnalyzedTypeId>,
) -> AnalyzerResult<usize> {
    let actual_type_id = resolve_generic_type(type_id, generic_params, generic_args);
    match actual_type_id {
        AnalyzedTypeId::StructType(id, _) => {
            if !visited.insert(id.clone()) {
                return Err(anyhow::anyhow!("Type cycle detected: {:?}", type_id));
            }
            let resolved_struct = resolved_structs.get(&id).unwrap();
            let mut size = 0;
            for field_name in &resolved_struct.field_order {
                let field_type = resolved_struct.field_types.get(field_name).unwrap();
                let actual_field_type =
                    resolve_generic_type(field_type, generic_params, generic_args);
                size += compute_type_size(
                    &actual_field_type,
                    resolved_structs,
                    visited,
                    generic_params,
                    generic_args,
                )?;
            }
            Ok(size)
        }
        AnalyzedTypeId::Pointer(_) => Ok(8),
        AnalyzedTypeId::Unit => Ok(0),
        AnalyzedTypeId::Bool => Ok(1),
        AnalyzedTypeId::Char => Ok(1),
        AnalyzedTypeId::Integer(size) => Ok(size),
        AnalyzedTypeId::GenericType(name) => unreachable!("Generic type {} should be resolved", name),
    }
}

pub fn get_type_size(
    type_id: &AnalyzedTypeId,
    struct_sizes: &HashMap<AnalyzedTypeId, usize>,
) -> usize {
    match type_id {
        AnalyzedTypeId::StructType(_, _) => *struct_sizes.get(type_id).unwrap(),
        AnalyzedTypeId::Pointer(_) => 8,
        AnalyzedTypeId::Unit => 0,
        AnalyzedTypeId::Bool => 1,
        AnalyzedTypeId::Char => 1,
        AnalyzedTypeId::Integer(size) => *size,
        AnalyzedTypeId::GenericType(_) => {
            unreachable!("Generic type {} should be resolved", type_id)
        }
    }
}
