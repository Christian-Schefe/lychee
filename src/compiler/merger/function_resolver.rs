use crate::compiler::builtin;
use crate::compiler::merger::merged_expression::{
    ModuleId, ResolvedFunctionHeader, ResolvedFunctions, ResolvedTypes,
};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{ParsedModule, ParsedProgram};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub fn build_resolved_functions(
    program: &ParsedProgram,
    resolved_types: &ResolvedTypes,
) -> MergerResult<ResolvedFunctions> {
    let mut functions = HashMap::new();
    builtin::BuiltinFunction::add_builtin_function_headers(&mut functions);

    let mut builtin_functions = HashMap::new();
    for (builtin_fn, _) in &functions {
        builtin_functions.insert(builtin_fn.name.clone(), builtin_fn.clone());
    }

    extract_module_functions(resolved_types, &mut functions, &program.module_tree)?;

    Ok(ResolvedFunctions {
        functions,
        builtin_functions,
    })
}

fn extract_module_functions(
    resolved_types: &ResolvedTypes,
    functions: &mut HashMap<ModuleId, ResolvedFunctionHeader>,
    module_tree: &HashMap<ModuleIdentifier, ParsedModule>,
) -> MergerResult<()> {
    for module in module_tree.values() {
        for func_def in &module.functions {
            let func_id = ModuleId {
                name: func_def.value.function_name.clone(),
                module_path: module.module_path.clone(),
            };

            let return_type =
                resolved_types.resolve_type(&module.module_path, &func_def.value.return_type)?;

            let mut parameter_order = Vec::with_capacity(func_def.value.args.len());
            let mut parameter_types = HashMap::with_capacity(func_def.value.args.len());

            for (arg_type, arg_name) in &func_def.value.args {
                let arg_type = resolved_types.resolve_type(&module.module_path, arg_type)?;
                parameter_order.push(arg_name.clone());
                parameter_types.insert(arg_name.clone(), arg_type);
            }

            let header = ResolvedFunctionHeader {
                id: func_id.clone(),
                return_type,
                parameter_order,
                parameter_types,
            };

            if functions.insert(func_id.clone(), header).is_some() {
                return Err(anyhow::anyhow!(
                    "Duplicate function definition: {}",
                    func_id.name
                ));
            }
        }
    }
    Ok(())
}
