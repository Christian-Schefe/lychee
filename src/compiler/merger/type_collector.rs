use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{GenericParams, ParsedProgram, ParsedTypeKind};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CollectedTypeData {
    pub structs: HashMap<ModuleIdentifier, HashMap<String, ItemId>>,
    pub struct_imports: HashMap<ModuleIdentifier, HashMap<String, ItemId>>,
}

impl CollectedTypeData {
    pub fn map_parsed_type(&self, parsed_type: &ParsedTypeKind) -> Option<AnalyzedTypeId> {
        match &parsed_type {
            ParsedTypeKind::Struct(id, generic_args) => {
                let module_structs = self.structs.get(&id.item_id.module_id).unwrap();
                let imported_structs = self.struct_imports.get(&id.item_id.module_id).unwrap();
                let mapped_generic_args = generic_args
                    .iter()
                    .map(|x| self.map_parsed_type(&x.value))
                    .collect::<Option<Vec<AnalyzedTypeId>>>()?;
                if let Some(id) = module_structs.get(&id.item_id.item_name) {
                    return Some(AnalyzedTypeId::StructType(id.clone(), mapped_generic_args));
                }
                if id.is_module_local {
                    if let Some(id) = imported_structs.get(&id.item_id.item_name) {
                        return Some(AnalyzedTypeId::StructType(id.clone(), mapped_generic_args));
                    }
                }
                None
            }
            ParsedTypeKind::Pointer(inner) => Some(AnalyzedTypeId::Pointer(Box::new(
                self.map_parsed_type(inner).unwrap(),
            ))),
            ParsedTypeKind::Unit => Some(AnalyzedTypeId::Unit),
            ParsedTypeKind::Bool => Some(AnalyzedTypeId::Bool),
            ParsedTypeKind::Char => Some(AnalyzedTypeId::Char),
            ParsedTypeKind::Integer(size) => Some(AnalyzedTypeId::Integer(*size)),
        }
    }

    pub fn map_generic_parsed_type(
        &self,
        parsed_type: &ParsedTypeKind,
        generic_params: &Option<GenericParams>,
    ) -> Option<AnalyzedTypeId> {
        match &parsed_type {
            ParsedTypeKind::Struct(id, generic_args) => {
                let module_structs = self.structs.get(&id.item_id.module_id).unwrap();
                let imported_structs = self.struct_imports.get(&id.item_id.module_id).unwrap();
                let mapped_generic_args = generic_args
                    .iter()
                    .map(|x| self.map_generic_parsed_type(&x.value, generic_params))
                    .collect::<Option<Vec<AnalyzedTypeId>>>()?;
                if id.is_module_local && generic_args.len() == 0 {
                    if let Some(generic_arg) = generic_params
                        .as_ref()
                        .and_then(|x| x.set.get(&id.item_id.item_name))
                    {
                        return Some(AnalyzedTypeId::GenericType(generic_arg.clone()));
                    }
                }
                if let Some(id) = module_structs.get(&id.item_id.item_name) {
                    return Some(AnalyzedTypeId::StructType(id.clone(), mapped_generic_args));
                }
                if id.is_module_local {
                    if let Some(id) = imported_structs.get(&id.item_id.item_name) {
                        return Some(AnalyzedTypeId::StructType(id.clone(), mapped_generic_args));
                    }
                }
                None
            }
            ParsedTypeKind::Pointer(inner) => Some(AnalyzedTypeId::Pointer(Box::new(
                self.map_generic_parsed_type(inner, generic_params).unwrap(),
            ))),
            ParsedTypeKind::Unit => Some(AnalyzedTypeId::Unit),
            ParsedTypeKind::Bool => Some(AnalyzedTypeId::Bool),
            ParsedTypeKind::Char => Some(AnalyzedTypeId::Char),
            ParsedTypeKind::Integer(size) => Some(AnalyzedTypeId::Integer(*size)),
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
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, ItemId>>> {
    let mut structs = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_types = HashMap::new();
        for struct_def in &module.struct_definitions {
            validate_struct_name(&struct_def.value.struct_name)?;
            let id = ItemId {
                module_id: module_id.clone(),
                item_name: struct_def.value.struct_name.clone(),
            };
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
    structs: &HashMap<ModuleIdentifier, HashMap<String, ItemId>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, ItemId>>> {
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
            } else {
                for (name, id) in module_structs {
                    if module_struct_imports
                        .insert(name.clone(), id.clone())
                        .is_some()
                    {
                        return Err(anyhow::anyhow!(
                            "Duplicate struct import '{}' at {}",
                            name,
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
