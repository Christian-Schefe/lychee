use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::unwrapper::unwrapped_type::{UnwrappedStruct, UnwrappedTypeId};
use std::collections::{HashMap, HashSet};

pub struct StructInformation {
    pub struct_sizes: HashMap<String, usize>,
    pub field_offsets: HashMap<String, HashMap<String, usize>>,
}

pub fn resolve_structs(structs: &HashMap<String, UnwrappedStruct>) -> StructInformation {
    let mut struct_sizes = HashMap::new();
    compute_type_sizes(structs, &mut struct_sizes);

    let mut field_offsets = HashMap::new();
    for (id, resolved_struct) in structs {
        let mut offsets = HashMap::new();
        let mut offset = 0;
        for field_name in resolved_struct.field_order.iter().rev() {
            let field_type = resolved_struct.field_types.get(field_name).unwrap();
            offsets.insert(field_name.clone(), offset);
            offset += get_type_size(field_type, &struct_sizes);
        }
        field_offsets.insert(id.clone(), offsets);
    }

    StructInformation {
        struct_sizes,
        field_offsets,
    }
}

fn compute_type_sizes(
    structs: &HashMap<String, UnwrappedStruct>,
    struct_sizes: &mut HashMap<String, usize>,
) {
    for (id, _) in structs {
        let mut visiting = HashSet::new();
        let size = compute_type_size(
            &UnwrappedTypeId::StructType(id.clone()),
            structs,
            &mut visiting,
            struct_sizes,
        )
        .unwrap();
        struct_sizes.insert(id.clone(), size);
    }
}

fn compute_type_size(
    type_id: &UnwrappedTypeId,
    structs: &HashMap<String, UnwrappedStruct>,
    visiting: &mut HashSet<String>,
    visited: &mut HashMap<String, usize>,
) -> AnalyzerResult<usize> {
    match type_id {
        UnwrappedTypeId::StructType(id) => {
            if let Some(size) = visited.get(id) {
                return Ok(*size);
            }
            if !visiting.insert(id.clone()) {
                return Err(anyhow::anyhow!("Type cycle detected: {:?}", type_id));
            }
            let resolved_struct = structs.get(id).unwrap();
            let mut size = 0;
            for field_name in &resolved_struct.field_order {
                let field_type = resolved_struct.field_types.get(field_name).unwrap();
                size += compute_type_size(&field_type, structs, visiting, visited)?;
            }
            visiting.remove(id);
            visited.insert(id.clone(), size);
            Ok(size)
        }
        UnwrappedTypeId::Pointer(_) => Ok(8),
        UnwrappedTypeId::Unit => Ok(0),
        UnwrappedTypeId::Bool => Ok(1),
        UnwrappedTypeId::Char => Ok(1),
        UnwrappedTypeId::Integer(size) => Ok(*size),
        UnwrappedTypeId::FunctionType(_, _) => Ok(8),
    }
}

pub fn get_type_size(type_id: &UnwrappedTypeId, struct_sizes: &HashMap<String, usize>) -> usize {
    match type_id {
        UnwrappedTypeId::StructType(id) => *struct_sizes.get(id).unwrap(),
        UnwrappedTypeId::Pointer(_) => 8,
        UnwrappedTypeId::Unit => 0,
        UnwrappedTypeId::Bool => 1,
        UnwrappedTypeId::Char => 1,
        UnwrappedTypeId::Integer(size) => *size,
        UnwrappedTypeId::FunctionType(_, _) => 8,
    }
}
