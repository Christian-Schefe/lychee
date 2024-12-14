use crate::compiler::merger::merged_expression::TypeId;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{ParsedProgram, ParsedType, ParsedTypeKind};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CollectedTypeData {
    pub structs: HashMap<ModuleIdentifier, HashMap<String, TypeId>>,
    pub struct_imports: HashMap<ModuleIdentifier, HashMap<String, TypeId>>,
}

impl CollectedTypeData {
    pub fn map_parsed_type(&self, parsed_type: &ParsedType) -> Option<TypeId> {
        match &parsed_type.value {
            ParsedTypeKind::Struct(id) => {
                let module_structs = self.structs.get(&id.item_id.module_id).unwrap();
                let imported_structs = self.struct_imports.get(&id.item_id.module_id).unwrap();
                if let Some(id) = module_structs.get(&id.item_id.item_name) {
                    return Some(id.clone());
                }
                if id.is_module_local {
                    if let Some(id) = imported_structs.get(&id.item_id.item_name) {
                        return Some(id.clone());
                    }
                }
                println!("{:?}", module_structs);
                println!("{:?}", imported_structs);
                println!("{:?}", id);
                None
            }
            ParsedTypeKind::Pointer(inner) => {
                Some(TypeId::Pointer(Box::new(self.map_parsed_type(inner)?)))
            }
            ParsedTypeKind::Unit => Some(TypeId::Unit),
            ParsedTypeKind::Bool => Some(TypeId::Bool),
            ParsedTypeKind::Char => Some(TypeId::Char),
            ParsedTypeKind::Integer(size) => Some(TypeId::Integer(*size)),
        }
    }
}

pub fn collect_type_data(program: &ParsedProgram) -> MergerResult<CollectedTypeData> {
    let structs = collect_structs(program)?;
    let struct_imports = collect_struct_imports(program, &structs)?;
    Ok(CollectedTypeData {
        structs,
        struct_imports,
    })
}

fn collect_structs(
    program: &ParsedProgram,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, TypeId>>> {
    let mut structs = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_types = HashMap::new();
        for struct_def in &module.struct_definitions {
            validate_struct_name(&struct_def.value.struct_name)?;
            let id = TypeId::StructType(ItemId {
                module_id: module_id.clone(),
                item_name: struct_def.value.struct_name.clone(),
            });
            if module_types
                .insert(struct_def.value.struct_name.clone(), id)
                .is_some()
            {
                return Err(anyhow::anyhow!(
                    "Duplicate struct definition '{}' at {}",
                    struct_def.value.struct_name,
                    struct_def.location
                ));
            }
        }
        structs.insert(module_id.clone(), module_types);
    }
    Ok(structs)
}

fn collect_struct_imports(
    program: &ParsedProgram,
    structs: &HashMap<ModuleIdentifier, HashMap<String, TypeId>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, TypeId>>> {
    let mut struct_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_struct_imports = HashMap::new();
        for import in &module.imports {
            if import.value.impl_type.is_some() {
                continue;
            }
            let module_structs = structs.get(&import.value.module_id).unwrap();
            if let Some(obj) = &import.value.imported_object {
                if let Some(id) = module_structs.get(obj) {
                    if module_struct_imports
                        .insert(obj.clone(), id.clone())
                        .is_some()
                    {
                        return Err(anyhow::anyhow!(
                            "Duplicate struct import '{}' at {}",
                            obj,
                            import.location
                        ));
                    }
                }
            }
        }
        struct_imports.insert(module_id.clone(), module_struct_imports);
    }
    Ok(struct_imports)
}

fn validate_struct_name(struct_name: &str) -> MergerResult<()> {
    let builtin_types = ["unit", "bool", "char", "byte", "short", "int", "long"];
    if builtin_types.contains(&struct_name) {
        Err(anyhow::anyhow!(
            "Type '{}' is a builtin type and cannot be redefined",
            struct_name
        ))
    } else {
        Ok(())
    }
}
