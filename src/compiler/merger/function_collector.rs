use crate::compiler::merger::merged_expression::FunctionId;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::item_id::{ItemId, ParsedFunctionId};
use crate::compiler::parser::parsed_expression::ParsedProgram;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CollectedFunctionData {
    pub functions: HashMap<ModuleIdentifier, HashMap<String, Vec<FunctionId>>>,
    pub function_imports: HashMap<ModuleIdentifier, HashMap<String, Vec<FunctionId>>>,
    pub builtin_functions: HashMap<String, FunctionId>,
}

impl CollectedFunctionData {
    pub fn find_function_id(
        &self,
        parsed_function_id: &ParsedFunctionId,
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

pub fn collect_function_data(program: &ParsedProgram) -> MergerResult<CollectedFunctionData> {
    let mut builtin_functions = HashMap::new();
    crate::compiler::builtin::BuiltinFunction::add_builtin_function_ids(&mut builtin_functions);
    let functions = collect_functions(program)?;
    let function_imports = collect_function_imports(program, &functions)?;
    Ok(CollectedFunctionData {
        functions,
        function_imports,
        builtin_functions,
    })
}

fn collect_functions(
    program: &ParsedProgram,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, Vec<FunctionId>>>> {
    let mut functions = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_functions = HashMap::new();
        for function_def in &module.functions {
            validate_function_name(&function_def.value.function_name)?;
            let id = FunctionId {
                id: ItemId {
                    module_id: module_id.clone(),
                    item_name: function_def.value.function_name.clone(),
                },
                generic_count: function_def.value.generic_params.order.len(),
                param_count: function_def.value.params.len(),
            };
            let entry = module_functions
                .entry(function_def.value.function_name.clone())
                .or_insert(vec![]);
            entry.push(id);
        }
        functions.insert(module_id.clone(), module_functions);
    }
    Ok(functions)
}

fn collect_function_imports(
    program: &ParsedProgram,
    functions: &HashMap<ModuleIdentifier, HashMap<String, Vec<FunctionId>>>,
) -> MergerResult<HashMap<ModuleIdentifier, HashMap<String, Vec<FunctionId>>>> {
    let mut function_imports = HashMap::new();
    for (module_id, module) in &program.module_tree {
        let mut module_function_imports = HashMap::new();
        for import in &module.imports {
            let module_functions = functions.get(&import.value.module_id).unwrap();
            if let Some(obj) = &import.value.imported_object {
                let entry = module_function_imports.entry(obj.clone()).or_insert(vec![]);
                if let Some(ids) = module_functions.get(obj) {
                    entry.extend(ids.clone());
                }
            } else {
                for (name, id) in module_functions {
                    let entry = module_function_imports
                        .entry(name.clone())
                        .or_insert(vec![]);
                    entry.extend(id.clone());
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
