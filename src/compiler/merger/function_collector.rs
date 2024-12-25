use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::FunctionId;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::{ItemId, ParsedScopeId};
use crate::compiler::parser::parsed_expression::{ParsedFunction, ParsedProgram};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct CollectedFunctionData {
    pub functions: HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>,
    pub function_imports: HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>,
    pub builtin_functions: HashMap<String, FunctionId>,
}

impl CollectedFunctionData {
    pub fn find_function_id(
        &self,
        parsed_function_id: &ParsedScopeId,
        arg_count: usize,
        generic_count: usize,
    ) -> Option<Vec<FunctionId>> {
        let module_functions = self.functions.get(&parsed_function_id.item_id.module_id)?;
        let module_function_imports = self
            .function_imports
            .get(&parsed_function_id.item_id.module_id)?;

        let mut matching = Vec::new();

        if let Some(function_ids) = module_functions.get(&parsed_function_id.item_id.item_name) {
            for function_id in function_ids {
                if function_id.generic_count == generic_count
                    && function_id.param_count == arg_count
                {
                    matching.push(function_id.clone());
                }
            }
        }

        if parsed_function_id.is_module_local {
            if let Some(function_ids) =
                module_function_imports.get(&parsed_function_id.item_id.item_name)
            {
                for function_id in function_ids {
                    if function_id.generic_count == generic_count
                        && function_id.param_count == arg_count
                    {
                        matching.push(function_id.clone());
                    }
                }
            }
            if let Some(builtin_fn_id) = self
                .builtin_functions
                .get(&parsed_function_id.item_id.item_name)
            {
                if builtin_fn_id.generic_count == generic_count
                    && builtin_fn_id.param_count == arg_count
                {
                    matching.push(builtin_fn_id.clone());
                }
            }
        }

        Some(matching)
    }
}

pub fn collect_function_data(
    program: &ParsedProgram,
) -> MergerResult<(
    CollectedFunctionData,
    Vec<(FunctionId, Src<ParsedFunction>)>,
)> {
    let builtin_functions = crate::compiler::builtin::BuiltinFunction::get_builtin_function_ids();
    let mut function_bodies = Vec::new();
    let functions = collect_functions(program, &mut function_bodies)?;
    let function_imports = collect_function_imports(program, &functions)?;
    Ok((
        CollectedFunctionData {
            functions,
            function_imports,
            builtin_functions,
        },
        function_bodies,
    ))
}

fn collect_functions(
    program: &ParsedProgram,
    function_bodies: &mut Vec<(FunctionId, Src<ParsedFunction>)>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>> {
    let mut functions = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_functions = HashMap::new();
        for function_def in &module.functions {
            validate_function_name(&function_def.value.function_name)?;
            let body_index = function_bodies.len();
            let id = FunctionId {
                id: ItemId {
                    module_id: module_id.clone(),
                    item_name: function_def.value.function_name.clone(),
                },
                generic_count: function_def.value.generic_params.order.len(),
                param_count: function_def.value.params.len(),
                body_index,
            };
            let entry = module_functions
                .entry(function_def.value.function_name.clone())
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
    Ok(functions)
}

fn collect_function_imports(
    program: &ParsedProgram,
    functions: &HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, HashSet<FunctionId>>>> {
    let mut function_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_function_imports = HashMap::new();
        for import in &module.imports {
            let module_functions = functions.get(&import.value.module_id).unwrap();
            if let Some(objects) = &import.value.imported_objects {
                for obj in objects {
                    let entry = module_function_imports
                        .entry(obj.clone())
                        .or_insert(HashSet::new());
                    if let Some(ids) = module_functions.get(obj) {
                        for id in ids {
                            if !entry.insert(id.clone()) {
                                Err(anyhow::anyhow!(
                                    "Function {} imported multiple times at {}",
                                    id.id.item_name,
                                    import.location
                                ))?;
                            }
                        }
                    }
                }
            } else {
                for (name, ids) in module_functions {
                    let entry = module_function_imports
                        .entry(name.clone())
                        .or_insert(HashSet::new());
                    for id in ids {
                        if !entry.insert(id.clone()) {
                            Err(anyhow::anyhow!(
                                "Function {} imported multiple times at {}",
                                id.id.item_name,
                                import.location
                            ))?;
                        }
                    }
                }
            }
        }
        function_imports.insert(module_id.clone(), module_function_imports);
    }
    Ok(function_imports)
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
