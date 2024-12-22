use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::merger::merged_expression::{StructId, StructRef};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::{ItemId, ParsedTypeId};
use crate::compiler::parser::parsed_expression::{GenericParams, ParsedProgram, ParsedTypeKind};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CollectedTypeData {
    pub structs: HashMap<ModuleIdentifier, HashMap<String, Vec<StructId>>>,
    pub struct_imports: HashMap<ModuleIdentifier, HashMap<String, Vec<StructId>>>,
}

impl CollectedTypeData {
    pub fn find_struct_ids(
        &self,
        id: &ParsedTypeId,
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
                    if let Some(generic_param) = generic_params.get_generic(&id.item_id.item_name) {
                        return Some(generic_param);
                    }
                }

                let struct_ids = self.find_struct_ids(id, generic_args.len())?;
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
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, Vec<StructId>>>> {
    let mut structs = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_types = HashMap::new();
        for struct_def in &module.struct_definitions {
            validate_struct_name(&struct_def.value.struct_name)?;
            let id = ItemId {
                module_id: module_id.clone(),
                item_name: struct_def.value.struct_name.clone(),
            };
            let struct_id = StructId {
                id,
                generic_count: struct_def.value.generics.order.len(),
            };
            let entry = module_types
                .entry(struct_def.value.struct_name.clone())
                .or_insert(vec![]);
            entry.push(struct_id);
        }
        structs.insert(module_id.clone(), module_types);
    }
    Ok(structs)
}

fn collect_struct_imports(
    program: &ParsedProgram,
    structs: &HashMap<ModuleIdentifier, HashMap<String, Vec<StructId>>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, Vec<StructId>>>> {
    let mut struct_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_struct_imports = HashMap::new();
        for import in &module.imports {
            let module_structs = structs.get(&import.value.module_id).unwrap();
            if let Some(obj) = &import.value.imported_object {
                let entry = module_struct_imports.entry(obj.clone()).or_insert(vec![]);
                if let Some(ids) = module_structs.get(obj) {
                    entry.extend(ids.clone());
                }
            } else {
                for (name, ids) in module_structs {
                    let entry = module_struct_imports.entry(name.clone()).or_insert(vec![]);
                    entry.extend(ids.clone());
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
