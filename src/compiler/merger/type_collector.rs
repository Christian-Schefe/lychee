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
}

impl CollectedTypeData {
    pub fn find_struct_ids(&self, id: &ParsedScopeId, generic_count: usize) -> Vec<StructId> {
        let mut matching = Vec::new();

        let check_add = |matching: &mut Vec<StructId>, ids: Option<&HashSet<StructId>>| {
            if let Some(ids) = ids {
                for id in ids {
                    if id.generic_count == generic_count {
                        matching.push(id.clone());
                    }
                }
            }
        };

        if let Some(module_structs) = self.structs.get(&id.item_id.module_id) {
            check_add(&mut matching, module_structs.get(&id.item_id.item_name));
        }
        if id.is_module_local {
            if let Some(imported_structs) = self.struct_imports.get(&id.item_id.module_id) {
                check_add(&mut matching, imported_structs.get(&id.item_id.item_name));
            }
        }
        matching
    }

    pub fn find_enum_ids(&self, id: &ParsedScopeId) -> Vec<ItemId> {
        let mut matching = Vec::new();
        if let Some(module_enums) = self.enums.get(&id.item_id.module_id) {
            if let Some(enum_id) = module_enums.get(&id.item_id.item_name) {
                matching.push(enum_id.clone());
            }
        }
        if id.is_module_local {
            if let Some(imported_enums) = self.enum_imports.get(&id.item_id.module_id) {
                if let Some(enum_id) = imported_enums.get(&id.item_id.item_name) {
                    matching.push(enum_id.clone());
                }
            }
        }
        matching
    }

    pub fn map_generic_parsed_type(
        &self,
        ty: &ParsedTypeKind,
        generic_params: &GenericParams,
    ) -> Option<AnalyzedTypeId> {
        match ty {
            ParsedTypeKind::Struct(id) => {
                let item_id = &id.id.item_id;
                if id.id.is_module_local {
                    if let Some(generic_param) =
                        generic_params.get_generic_from_name(&item_id.item_name)
                    {
                        return Some(AnalyzedTypeId::GenericType(generic_param.clone()));
                    }

                    if let Some(module_aliases) = self.type_aliases.get(&item_id.module_id) {
                        if let Some(alias) = module_aliases.get(&item_id.item_name) {
                            return self.map_generic_parsed_type(&alias.value, generic_params);
                        }
                    }
                    if let Some(imported_aliases) =
                        self.imported_type_aliases.get(&item_id.module_id)
                    {
                        if let Some(alias) = imported_aliases.get(&item_id.item_name) {
                            return self.map_generic_parsed_type(&alias.value, generic_params);
                        }
                    }
                }

                let generic_args_len = id.generic_args.len();

                let struct_ids = self.find_struct_ids(&id.id, generic_args_len);

                if generic_args_len == 0 {
                    let mut enum_ids = self.find_enum_ids(&id.id);
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
                let mapped_generic_args = id
                    .generic_args
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
    let mut structs = HashMap::new();
    builtin::BuiltinStruct::get_builtin_struct_ids(&mut structs);
    collect_structs(program, &mut structs)?;
    let struct_imports = collect_struct_imports(program, &structs)?;
    let type_aliases = collect_type_aliases(program)?;
    let imported_type_aliases = collect_type_alias_imports(program, &type_aliases)?;
    let enums = collect_enums(program)?;
    let enum_imports = collect_enum_imports(program, &enums)?;
    Ok(CollectedTypeData {
        structs,
        struct_imports,
        type_aliases,
        imported_type_aliases,
        enums,
        enum_imports,
    })
}

fn collect_structs(
    program: &ParsedProgram,
    structs: &mut HashMap<ModuleIdentifier, HashMap<String, HashSet<StructId>>>,
) -> MergerResult<()> {
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
    Ok(())
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
            let module_structs = match structs.get(&import.value.module_id) {
                Some(s) => s,
                None => continue,
            };
            let mut add_ids = |obj: &String, ids: &HashSet<StructId>| {
                let entry = module_struct_imports
                    .entry(obj.clone())
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
                Ok(())
            };
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(ids) = module_structs.get(obj) {
                        add_ids(obj, ids)?;
                    }
                }
            } else {
                for (name, ids) in module_structs {
                    add_ids(name, ids)?;
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
            let module_type_aliases = match type_aliases.get(&import.value.module_id) {
                Some(aliases) => aliases,
                None => continue,
            };
            let mut add_alias = |obj: &String, alias: &ParsedType| {
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
                Ok(())
            };
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(alias) = module_type_aliases.get(obj) {
                        add_alias(obj, alias)?;
                    }
                }
            } else {
                for (name, alias) in module_type_aliases {
                    add_alias(name, alias)?;
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
            let module_enums = match enums.get(&import.value.module_id) {
                Some(enums) => enums,
                None => continue,
            };
            let mut add_enum = |obj: &String, enum_id: &ItemId| {
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
                Ok(())
            };
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(enum_id) = module_enums.get(obj) {
                        add_enum(obj, enum_id)?;
                    }
                }
            } else {
                for (name, enum_id) in module_enums {
                    add_enum(name, enum_id)?;
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
