use crate::compiler::analyzer::analyzed_type::{AnalyzedTypeId, GenericParams};
use crate::compiler::builtin;
use crate::compiler::merger::merged_expression::{StructId, StructRef};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::{ItemId, ParsedScopeId};
use crate::compiler::parser::parsed_expression::{ParsedProgram, ParsedType, ParsedTypeKind};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct CollectedTypeData {
    pub structs: HashMap<ModuleIdentifier, HashMap<String, HashSet<StructId>>>,
    pub struct_imports: HashMap<ModuleIdentifier, HashMap<String, HashSet<StructId>>>,
    pub type_aliases: HashMap<ModuleIdentifier, HashMap<String, ParsedType>>,
    pub imported_type_aliases: HashMap<ModuleIdentifier, HashMap<String, ParsedType>>,
    pub enums: HashMap<ModuleIdentifier, HashMap<String, ItemId>>,
    pub enum_imports: HashMap<ModuleIdentifier, HashMap<String, ItemId>>,
    pub builtin_structs: HashMap<String, HashSet<StructId>>,
}

impl CollectedTypeData {
    pub fn find_struct_ids(
        &self,
        id: &ParsedScopeId,
        generic_count: usize,
    ) -> Option<Vec<StructId>> {
        let module_structs = self.structs.get(&id.item_id.module_id)?;
        let imported_structs = self.struct_imports.get(&id.item_id.module_id)?;

        let mut matching = Vec::new();
        if let Some(struct_ids) = module_structs.get(&id.item_id.item_name) {
            for struct_id in struct_ids {
                if struct_id.generic_count == generic_count {
                    matching.push(struct_id.clone());
                }
            }
        }
        if id.is_module_local {
            if let Some(struct_ids) = imported_structs.get(&id.item_id.item_name) {
                for struct_id in struct_ids {
                    if struct_id.generic_count == generic_count {
                        matching.push(struct_id.clone());
                    }
                }
            }
            if let Some(builtin_struct_ids) = self.builtin_structs.get(&id.item_id.item_name) {
                for struct_id in builtin_struct_ids {
                    if struct_id.generic_count == generic_count {
                        matching.push(struct_id.clone());
                    }
                }
            }
        }
        Some(matching)
    }

    pub fn find_enum_ids(&self, id: &ParsedScopeId) -> Option<Vec<ItemId>> {
        let module_enums = self.enums.get(&id.item_id.module_id)?;
        let imported_enums = self.enum_imports.get(&id.item_id.module_id)?;

        let mut matching = Vec::new();
        if let Some(enum_id) = module_enums.get(&id.item_id.item_name) {
            matching.push(enum_id.clone());
        }
        if id.is_module_local {
            if let Some(enum_id) = imported_enums.get(&id.item_id.item_name) {
                matching.push(enum_id.clone());
            }
        }
        Some(matching)
    }

    pub fn map_generic_parsed_type(
        &self,
        ty: &ParsedTypeKind,
        generic_params: &GenericParams,
    ) -> Option<AnalyzedTypeId> {
        match ty {
            ParsedTypeKind::Struct(id, generic_args) => {
                if id.is_module_local {
                    if let Some(generic_param) =
                        generic_params.get_generic_from_name(&id.item_id.item_name)
                    {
                        return Some(AnalyzedTypeId::GenericType(generic_param.clone()));
                    }

                    let module_aliases = self.type_aliases.get(&id.item_id.module_id)?;
                    let imported_aliases = self.imported_type_aliases.get(&id.item_id.module_id)?;
                    if let Some(alias) = module_aliases.get(&id.item_id.item_name) {
                        return self.map_generic_parsed_type(&alias.value, generic_params);
                    }
                    if let Some(alias) = imported_aliases.get(&id.item_id.item_name) {
                        return self.map_generic_parsed_type(&alias.value, generic_params);
                    }
                }

                let struct_ids = self.find_struct_ids(id, generic_args.len())?;

                if generic_args.len() == 0 {
                    let mut enum_ids = self.find_enum_ids(id)?;
                    if enum_ids.len() > 0 {
                        if enum_ids.len() != 1 || struct_ids.len() != 0 {
                            return None;
                        }
                        return Some(AnalyzedTypeId::EnumType(enum_ids.pop().unwrap()));
                    }
                }

                if struct_ids.len() != 1 {
                    return None;
                }
                let mapped_generic_args = generic_args
                    .iter()
                    .map(|x| self.map_generic_parsed_type(&x.value, generic_params))
                    .collect::<Option<Vec<AnalyzedTypeId>>>()?;
                let struct_ref = StructRef {
                    id: struct_ids[0].clone(),
                    generic_args: mapped_generic_args,
                };
                Some(AnalyzedTypeId::StructType(struct_ref))
            }
            ParsedTypeKind::Pointer(inner) => {
                let inner = self.map_generic_parsed_type(inner, generic_params)?;
                Some(AnalyzedTypeId::Pointer(Box::new(inner)))
            }
            ParsedTypeKind::Unit => Some(AnalyzedTypeId::Unit),
            ParsedTypeKind::Bool => Some(AnalyzedTypeId::Bool),
            ParsedTypeKind::Char => Some(AnalyzedTypeId::Char),
            ParsedTypeKind::Integer(size) => Some(AnalyzedTypeId::Integer(*size)),
            ParsedTypeKind::Function {
                return_type,
                params,
            } => {
                let mapped_return =
                    self.map_generic_parsed_type(&return_type.value, generic_params)?;
                let mapped_params = params
                    .iter()
                    .map(|x| self.map_generic_parsed_type(&x.value, generic_params))
                    .collect::<Option<Vec<AnalyzedTypeId>>>()?;
                Some(AnalyzedTypeId::FunctionType(
                    Box::new(mapped_return),
                    mapped_params,
                ))
            }
        }
    }
}

pub fn collect_type_data(program: &ParsedProgram) -> MergerResult<CollectedTypeData> {
    let structs = collect_structs(program)?;
    let struct_imports = collect_struct_imports(program, &structs)?;
    let type_aliases = collect_type_aliases(program)?;
    let imported_type_aliases = collect_type_alias_imports(program, &type_aliases)?;
    let enums = collect_enums(program)?;
    let enum_imports = collect_enum_imports(program, &enums)?;
    let builtin_structs = builtin::BuiltinStruct::get_builtin_struct_ids();
    Ok(CollectedTypeData {
        structs,
        struct_imports,
        type_aliases,
        imported_type_aliases,
        enums,
        enum_imports,
        builtin_structs,
    })
}

fn collect_structs(
    program: &ParsedProgram,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, HashSet<StructId>>>> {
    let mut structs = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_types = HashMap::new();
        for struct_def in &module.struct_definitions {
            validate_struct_name(&struct_def.value.struct_name)?;
            let struct_id = StructId {
                id: ItemId {
                    module_id: module_id.clone(),
                    item_name: struct_def.value.struct_name.clone(),
                },
                generic_count: struct_def.value.generics.order.len(),
            };
            let entry = module_types
                .entry(struct_def.value.struct_name.clone())
                .or_insert(HashSet::new());
            if !entry.insert(struct_id.clone()) {
                return Err(anyhow::anyhow!(
                    "Duplicate struct definition: {} at {}",
                    struct_def.value.struct_name,
                    struct_def.location
                ));
            }
        }
        structs.insert(module_id.clone(), module_types);
    }
    Ok(structs)
}

fn collect_type_aliases(
    program: &ParsedProgram,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, ParsedType>>> {
    let mut type_aliases = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_aliases = HashMap::new();
        for alias in &module.type_aliases {
            if module_aliases
                .insert(alias.value.alias.clone(), alias.value.aliased_type.clone())
                .is_some()
            {
                return Err(anyhow::anyhow!(
                    "Duplicate type alias: {} at {}",
                    alias.value.alias,
                    alias.location
                ));
            }
        }
        type_aliases.insert(module_id.clone(), module_aliases);
    }
    Ok(type_aliases)
}

fn collect_enums(
    program: &ParsedProgram,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, ItemId>>> {
    let mut enums = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_enums = HashMap::new();
        for enum_def in &module.enums {
            let id = ItemId {
                module_id: module_id.clone(),
                item_name: enum_def.value.enum_name.clone(),
            };
            if module_enums
                .insert(enum_def.value.enum_name.clone(), id)
                .is_some()
            {
                return Err(anyhow::anyhow!(
                    "Duplicate enum definition: {} at {}",
                    enum_def.value.enum_name,
                    enum_def.location
                ));
            }
        }
        enums.insert(module_id.clone(), module_enums);
    }
    Ok(enums)
}

fn collect_struct_imports(
    program: &ParsedProgram,
    structs: &HashMap<ModuleIdentifier, HashMap<String, HashSet<StructId>>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, HashSet<StructId>>>> {
    let mut struct_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_struct_imports = HashMap::new();
        for import in &module.imports {
            let module_structs = structs.get(&import.value.module_id).unwrap_or_else(|| {
                panic!(
                    "Module {} not found at {}",
                    import.value.module_id.get_identifier(),
                    import.location
                )
            });
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    let entry = module_struct_imports
                        .entry(obj.clone())
                        .or_insert(HashSet::new());
                    if let Some(ids) = module_structs.get(obj) {
                        for id in ids {
                            if !entry.insert(id.clone()) {
                                return Err(anyhow::anyhow!(
                                    "Struct {} imported multiple times at {}",
                                    id.id.item_name,
                                    import.location
                                ));
                            }
                        }
                    }
                }
            } else {
                for (name, ids) in module_structs {
                    let entry = module_struct_imports
                        .entry(name.clone())
                        .or_insert(HashSet::new());
                    for id in ids {
                        if !entry.insert(id.clone()) {
                            return Err(anyhow::anyhow!(
                                "Struct {} imported multiple times at {}",
                                id.id.item_name,
                                import.location
                            ));
                        }
                    }
                }
            }
        }
        struct_imports.insert(module_id.clone(), module_struct_imports);
    }
    Ok(struct_imports)
}

fn collect_type_alias_imports(
    program: &ParsedProgram,
    type_aliases: &HashMap<ModuleIdentifier, HashMap<String, ParsedType>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, ParsedType>>> {
    let mut type_alias_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_type_alias_imports = HashMap::new();
        for import in &module.imports {
            let module_type_aliases = type_aliases.get(&import.value.module_id).unwrap();
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(alias) = module_type_aliases.get(obj) {
                        if module_type_alias_imports
                            .insert(obj.clone(), alias.clone())
                            .is_some()
                        {
                            return Err(anyhow::anyhow!(
                                "Type alias {} imported multiple times at {}",
                                obj,
                                import.location
                            ));
                        }
                    }
                }
            } else {
                for (name, alias) in module_type_aliases {
                    if module_type_alias_imports
                        .insert(name.clone(), alias.clone())
                        .is_some()
                    {
                        return Err(anyhow::anyhow!(
                            "Type alias {} imported multiple times at {}",
                            name,
                            import.location
                        ));
                    }
                }
            }
        }
        type_alias_imports.insert(module_id.clone(), module_type_alias_imports);
    }
    Ok(type_alias_imports)
}

fn collect_enum_imports(
    program: &ParsedProgram,
    enums: &HashMap<ModuleIdentifier, HashMap<String, ItemId>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, ItemId>>> {
    let mut enum_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_enum_imports = HashMap::new();
        for import in &module.imports {
            let module_enums = enums.get(&import.value.module_id).unwrap();
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(enum_id) = module_enums.get(obj) {
                        if module_enum_imports
                            .insert(obj.clone(), enum_id.clone())
                            .is_some()
                        {
                            return Err(anyhow::anyhow!(
                                "Enum {} imported multiple times at {}",
                                obj,
                                import.location
                            ));
                        }
                    }
                }
            } else {
                for (name, enum_id) in module_enums {
                    if module_enum_imports
                        .insert(name.clone(), enum_id.clone())
                        .is_some()
                    {
                        return Err(anyhow::anyhow!(
                            "Enum {} imported multiple times at {}",
                            name,
                            import.location
                        ));
                    }
                }
            }
        }
        enum_imports.insert(module_id.clone(), module_enum_imports);
    }
    Ok(enum_imports)
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
