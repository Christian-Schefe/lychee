use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::{FunctionId, TraitId};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::{ItemId, ParsedScopeId};
use crate::compiler::parser::parsed_expression::{ParsedFunction, ParsedProgram};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct CollectedFunctionData {
    pub functions: HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>,
    pub function_imports: HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>,
    pub traits: HashMap<ModuleIdentifier, HashMap<String, HashSet<TraitId>>>,
    pub trait_imports: HashMap<ModuleIdentifier, HashMap<String, HashSet<TraitId>>>,
}

impl CollectedFunctionData {
    pub fn find_function_id(
        &self,
        parsed_function_id: &ParsedScopeId,
        arg_count: Option<usize>,
        generic_count: Option<usize>,
    ) -> Vec<FunctionId> {
        let mut matching = Vec::new();

        let maybe_add = |matching: &mut Vec<FunctionId>, ids: &HashSet<FunctionId>| {
            for id in ids {
                if generic_count.is_none_or(|x| x == id.generic_count)
                    && arg_count.is_none_or(|x| x == id.param_count)
                {
                    matching.push(id.clone());
                }
            }
        };

        if let Some(module_functions) = self.functions.get(&parsed_function_id.item_id.module_id) {
            if let Some(function_ids) = module_functions.get(&parsed_function_id.item_id.item_name)
            {
                maybe_add(&mut matching, function_ids);
            }
        }

        if parsed_function_id.is_module_local {
            if let Some(module_function_imports) = self
                .function_imports
                .get(&parsed_function_id.item_id.module_id)
            {
                if let Some(function_ids) =
                    module_function_imports.get(&parsed_function_id.item_id.item_name)
                {
                    maybe_add(&mut matching, function_ids);
                }
            }
        }

        matching
    }

    pub fn find_trait_id(
        &self,
        parsed_trait_id: &ParsedScopeId,
        generic_count: Option<usize>,
    ) -> Vec<TraitId> {
        let mut matching = Vec::new();

        let maybe_add = |matching: &mut Vec<TraitId>, ids: &HashSet<TraitId>| {
            for id in ids {
                if generic_count.is_none_or(|x| x == id.generic_count) {
                    matching.push(id.clone());
                }
            }
        };

        if let Some(module_traits) = self.traits.get(&parsed_trait_id.item_id.module_id) {
            if let Some(trait_ids) = module_traits.get(&parsed_trait_id.item_id.item_name) {
                maybe_add(&mut matching, trait_ids);
            }
        }

        if parsed_trait_id.is_module_local {
            if let Some(module_trait_imports) =
                self.trait_imports.get(&parsed_trait_id.item_id.module_id)
            {
                if let Some(trait_ids) =
                    module_trait_imports.get(&parsed_trait_id.item_id.item_name)
                {
                    maybe_add(&mut matching, trait_ids);
                }
            }
        }

        matching
    }
}

pub fn collect_function_data(
    program: &ParsedProgram,
) -> MergerResult<(
    CollectedFunctionData,
    Vec<(FunctionId, Src<ParsedFunction>)>,
)> {
    let mut function_bodies = Vec::new();
    let mut functions = HashMap::new();

    crate::compiler::builtin::BuiltinFunction::get_builtin_function_ids(&mut functions);
    collect_functions(program, &mut functions, &mut function_bodies)?;
    let function_imports = collect_function_imports(program, &functions)?;
    let mut traits = HashMap::new();
    collect_trait_definitions(program, &mut traits)?;
    let trait_imports = collect_trait_imports(program, &traits)?;
    Ok((
        CollectedFunctionData {
            functions,
            function_imports,
            traits,
            trait_imports,
        },
        function_bodies,
    ))
}

fn collect_functions(
    program: &ParsedProgram,
    functions: &mut HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>,
    function_bodies: &mut Vec<(FunctionId, Src<ParsedFunction>)>,
) -> MergerResult<()> {
    for (module_id, module) in &program.module_tree {
        let mut module_functions = HashMap::new();
        for function_def in &module.functions {
            let signature = &function_def.value.signature;
            validate_function_name(&signature.function_name)?;
            let body_index = function_bodies.len();
            let id = FunctionId {
                id: ItemId {
                    module_id: module_id.clone(),
                    item_name: signature.function_name.clone(),
                },
                generic_count: signature.generic_params.order.len(),
                param_count: signature.params.len(),
                body_index: body_index as isize,
            };
            let entry = module_functions
                .entry(signature.function_name.clone())
                .or_insert(HashSet::new());
            if !entry.insert(id.clone()) {
                Err(anyhow::anyhow!(
                    "Function {} defined multiple times at {}",
                    id.id.item_name,
                    function_def.location
                ))?;
            }
            function_bodies.push((id.clone(), function_def.clone()));
        }
        functions.insert(module_id.clone(), module_functions);
    }
    Ok(())
}

fn collect_function_imports(
    program: &ParsedProgram,
    functions: &HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>> {
    let mut function_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_function_imports = HashMap::new();
        for import in &module.imports {
            let module_functions = match functions.get(&import.value.module_id) {
                Some(f) => f,
                None => continue,
            };
            let mut add_imported_function = |obj: &String, ids: &HashSet<FunctionId>| {
                let entry = module_function_imports
                    .entry(obj.clone())
                    .or_insert(HashSet::new());
                for id in ids {
                    if !entry.insert(id.clone()) {
                        return Err(anyhow::anyhow!(
                            "Function {} imported multiple times at {}",
                            id.id.item_name,
                            import.location
                        ));
                    }
                }
                Ok(())
            };
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(ids) = module_functions.get(obj) {
                        add_imported_function(obj, ids)?;
                    }
                }
            } else {
                for (name, ids) in module_functions {
                    add_imported_function(name, ids)?;
                }
            }
        }
        function_imports.insert(module_id.clone(), module_function_imports);
    }
    Ok(function_imports)
}

fn collect_trait_definitions(
    program: &ParsedProgram,
    trait_definitions: &mut HashMap<ModuleIdentifier, HashMap<String, HashSet<TraitId>>>,
) -> MergerResult<()> {
    for (module_id, module) in &program.module_tree {
        let mut module_trait_defs = HashMap::new();
        for trait_def in &module.trait_definitions {
            let id = TraitId {
                id: ItemId {
                    module_id: module_id.clone(),
                    item_name: trait_def.value.trait_name.clone(),
                },
                generic_count: trait_def.value.generics.order.len(),
            };
            let entry = module_trait_defs
                .entry(trait_def.value.trait_name.clone())
                .or_insert(HashSet::new());
            if !entry.insert(id.clone()) {
                Err(anyhow::anyhow!(
                    "Trait {} defined multiple times at {}",
                    id.id.item_name,
                    trait_def.location
                ))?;
            }
        }
        trait_definitions.insert(module_id.clone(), module_trait_defs);
    }
    Ok(())
}

fn collect_trait_imports(
    program: &ParsedProgram,
    traits: &HashMap<ModuleIdentifier, HashMap<String, HashSet<TraitId>>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, HashSet<TraitId>>>> {
    let mut trait_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_trait_imports = HashMap::new();
        for import in &module.imports {
            let module_traits = match traits.get(&import.value.module_id) {
                Some(f) => f,
                None => continue,
            };
            let mut add_imported_trait = |obj: &String, ids: &HashSet<TraitId>| {
                let entry = module_trait_imports
                    .entry(obj.clone())
                    .or_insert(HashSet::new());
                for id in ids {
                    if !entry.insert(id.clone()) {
                        return Err(anyhow::anyhow!(
                            "Trait {} imported multiple times at {}",
                            id.id.item_name,
                            import.location
                        ));
                    }
                }
                Ok(())
            };
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    if let Some(ids) = module_traits.get(obj) {
                        add_imported_trait(obj, ids)?;
                    }
                }
            } else {
                for (name, ids) in module_traits {
                    add_imported_trait(name, ids)?;
                }
            }
        }
        trait_imports.insert(module_id.clone(), module_trait_imports);
    }
    Ok(trait_imports)
}

fn validate_function_name(struct_name: &str) -> MergerResult<()> {
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
