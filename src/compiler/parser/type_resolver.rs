use std::collections::HashMap;
use crate::compiler::lexer::Location;
use crate::compiler::parser::parser_error::LocationError;
use crate::compiler::parser::syntax_tree::{Program, SrcStructDefinition};
use crate::compiler::parser::type_analyzer::ValidationResult;
use crate::compiler::parser::types::{SrcType, Type, UnknownType};

pub type ResolvedTypes = HashMap<String, Type>;

fn order_struct_definitions_topologically(map: &ResolvedTypes, program: &Program) -> ValidationResult<Vec<SrcStructDefinition>> {
    let mut structs = HashMap::new();
    for struct_def in &program.struct_definitions {
        structs.insert(struct_def.value.name.clone(), struct_def.clone());
    }
    let mut ordered_structs = Vec::new();
    let mut visited = HashMap::new();

    fn visit_struct_def(map: &ResolvedTypes, name: &String, origin: &Location, structs: &HashMap<String, SrcStructDefinition>, visited: &mut HashMap<String, bool>, ordered_structs: &mut Vec<SrcStructDefinition>) -> ValidationResult<()> {
        visited.insert(name.clone(), false);
        let struct_def = structs.get(name).ok_or(LocationError::msg(&format!("Struct '{}' not found.", name), origin))?;
        fn look_at_type(map: &ResolvedTypes, ty: &UnknownType, origin: &Location, structs: &HashMap<String, SrcStructDefinition>, visited: &mut HashMap<String, bool>, ordered_structs: &mut Vec<SrcStructDefinition>) -> ValidationResult<()> {
            match ty {
                UnknownType::Named(name) => {
                    if map.contains_key(name) {
                        Ok(())
                    } else if !visited.contains_key(name) {
                        visit_struct_def(map, name, origin, structs, visited, ordered_structs)
                    } else if visited.get(name).unwrap() == &false {
                        Err(LocationError::msg(&format!("Struct '{}' has a circular dependency.", name), origin))
                    } else {
                        Ok(())
                    }
                }
                UnknownType::Pointer(inner) => {
                    look_at_type(map, &inner.value, origin, structs, visited, ordered_structs)
                }
            }
        }
        for (_, ty) in &struct_def.value.fields {
            look_at_type(map, &ty.value, &struct_def.location, structs, visited, ordered_structs)?;
        }
        visited.insert(name.clone(), true);
        ordered_structs.push(struct_def.clone());
        Ok(())
    }

    for struct_def in &program.struct_definitions {
        if !visited.contains_key(&struct_def.value.name) {
            visit_struct_def(map, &struct_def.value.name, &struct_def.location, &structs, &mut visited, &mut ordered_structs)?;
        }
    }

    Ok(ordered_structs)
}

pub fn build_resolved_types(program: &Program) -> ValidationResult<ResolvedTypes> {
    let mut map = HashMap::new();

    map.insert("unit".to_string(), Type::Unit);
    map.insert("byte".to_string(), Type::Integer { size: 1 });
    map.insert("short".to_string(), Type::Integer { size: 2 });
    map.insert("int".to_string(), Type::Integer { size: 4 });
    map.insert("long".to_string(), Type::Integer { size: 8 });

    map.insert("char".to_string(), Type::Char);
    map.insert("bool".to_string(), Type::Bool);

    let ordered_structs = order_struct_definitions_topologically(&map, program)?;

    for def in ordered_structs {
        let mut resolved_fields = HashMap::with_capacity(def.value.fields.len());
        for (name, ty) in &def.value.fields {
            let resolved_ty = resolve_type(&map, ty)?;
            resolved_fields.insert(name.clone(), resolved_ty);
        }
        map.insert(def.value.name.clone(), Type::Struct {
            name: def.value.name.clone(),
            fields: resolved_fields,
        });
    };

    Ok(map)
}

pub fn resolve_type(map: &ResolvedTypes, unknown_type: &SrcType) -> ValidationResult<Type> {
    match &unknown_type.value {
        UnknownType::Named(name) => map.get(name).cloned().ok_or_else(|| LocationError::msg(&format!("Unknown type '{}'", name), &unknown_type.location)),
        UnknownType::Pointer(inner) => {
            let inner = resolve_type(map, inner)?;
            Ok(Type::Pointer(Box::new(inner)))
        }
    }
}