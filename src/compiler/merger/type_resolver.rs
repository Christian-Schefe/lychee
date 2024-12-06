use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::{ModuleId, ResolvedStruct, ResolvedTypes, TypeId};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{
    ParsedModule, ParsedProgram, ParsedStructDefinition, ParsedType, ParsedTypeKind,
};
use crate::compiler::parser::ModulePath;
use std::collections::{HashMap, HashSet};

pub fn build_resolved_types(parsed_program: &ParsedProgram) -> MergerResult<ResolvedTypes> {
    let mut struct_defs = HashMap::new();
    extract_module_struct_types(&mut struct_defs, &parsed_program.module_tree)?;

    let mut known_type_names = HashMap::new();
    let builtin_types = [
        ("unit".to_string(), TypeId::Unit),
        ("bool".to_string(), TypeId::Bool),
        ("char".to_string(), TypeId::Char),
        ("byte".to_string(), TypeId::Integer(1)),
        ("short".to_string(), TypeId::Integer(2)),
        ("int".to_string(), TypeId::Integer(4)),
        ("long".to_string(), TypeId::Integer(8)),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>();

    builtin_types.iter().for_each(|(name, type_id)| {
        known_type_names.insert(
            ModuleId {
                name: name.clone(),
                module_path: ModulePath(Vec::new()),
            },
            type_id.clone(),
        );
    });

    for (id, struct_def) in &struct_defs {
        let struct_id = ModuleId {
            name: struct_def.value.struct_name.clone(),
            module_path: id.module_path.clone(),
        };
        known_type_names.insert(struct_id.clone(), TypeId::StructType(struct_id));
    }

    let mut resolved_structs = HashMap::new();
    for (id, struct_def) in &struct_defs {
        let resolved_struct = resolve_struct_definition(struct_def.clone(), &known_type_names)?;
        let type_id = known_type_names.get(id).unwrap().clone();
        resolved_structs.insert(type_id, resolved_struct);
    }

    let type_sizes = compute_type_sizes(&known_type_names, &resolved_structs);

    compute_struct_field_offsets(&mut resolved_structs, &type_sizes);

    Ok(ResolvedTypes {
        structs: resolved_structs,
        type_sizes,
        known_types: known_type_names,
        builtin_types,
    })
}

fn compute_struct_field_offsets(
    resolved_structs: &mut HashMap<TypeId, ResolvedStruct>,
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

fn map_parsed_type(
    known_types: &HashMap<ModuleId, TypeId>,
    parsed_type: &ParsedType,
) -> MergerResult<TypeId> {
    match &parsed_type.value {
        ParsedTypeKind::Named(module_id) => known_types.get(&module_id).cloned().ok_or_else(|| {
            anyhow::Error::msg(format!(
                "Unknown type '{}' at {}",
                module_id.name, parsed_type.location
            ))
        }),
        ParsedTypeKind::Pointer(inner) => Ok(TypeId::Pointer(Box::new(map_parsed_type(
            known_types,
            inner,
        )?))),
    }
}

fn resolve_struct_definition(
    struct_def: Src<ParsedStructDefinition>,
    known_types: &HashMap<ModuleId, TypeId>,
) -> MergerResult<ResolvedStruct> {
    let mut field_types = HashMap::new();
    let mut field_order = Vec::new();

    for (field_name, field_type) in &struct_def.value.fields {
        field_order.push(field_name.clone());
        field_types.insert(
            field_name.clone(),
            map_parsed_type(known_types, field_type)?,
        );
    }

    Ok(ResolvedStruct {
        field_types,
        field_order,
        field_offsets: HashMap::new(),
    })
}

fn extract_module_struct_types(
    structs: &mut HashMap<ModuleId, Src<ParsedStructDefinition>>,
    module_tree: &HashMap<ModulePath, ParsedModule>,
) -> MergerResult<()> {
    for module in module_tree.values() {
        for struct_def in &module.struct_definitions {
            let struct_id = ModuleId {
                name: struct_def.value.struct_name.clone(),
                module_path: module.module_path.clone(),
            };

            if let Some(_) = structs.insert(struct_id.clone(), struct_def.clone()) {
                return Err(anyhow::anyhow!(
                    "Duplicate struct definition '{}' at {}",
                    struct_def.value.struct_name.clone(),
                    struct_def.location
                ));
            }
        }
    }
    Ok(())
}

fn compute_type_sizes(
    known_types: &HashMap<ModuleId, TypeId>,
    resolved_structs: &HashMap<TypeId, ResolvedStruct>,
) -> HashMap<TypeId, usize> {
    let mut type_sizes = HashMap::new();

    for (_, type_id) in known_types {
        let mut visited = HashSet::new();
        let size = compute_type_size(type_id, resolved_structs, known_types, &mut visited).unwrap();
        type_sizes.insert(type_id.clone(), size);
    }

    type_sizes
}

fn compute_type_size(
    type_id: &TypeId,
    resolved_structs: &HashMap<TypeId, ResolvedStruct>,
    known_types: &HashMap<ModuleId, TypeId>,
    visited: &mut HashSet<TypeId>,
) -> MergerResult<usize> {
    match type_id {
        TypeId::StructType(id) => {
            if !visited.insert(type_id.clone()) {
                return Err(anyhow::anyhow!("Type cycle detected: {:?}", type_id));
            }
            let type_id = known_types.get(id).unwrap();
            let resolved_struct = resolved_structs.get(type_id).unwrap();
            let mut size = 0;
            for field_name in &resolved_struct.field_order {
                let field_type = resolved_struct.field_types.get(field_name).unwrap();
                size += compute_type_size(field_type, resolved_structs, known_types, visited)?;
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
