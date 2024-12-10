use crate::compiler::builtin;
use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::{
    ModuleId, ResolvedFunctionHeader, ResolvedFunctions, ResolvedTypes, TypeId,
};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{ParsedFunction, ParsedModule, ParsedProgram};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub fn build_resolved_functions(
    program: &ParsedProgram,
    resolved_types: &ResolvedTypes,
) -> MergerResult<ResolvedFunctions> {
    let mut functions = HashMap::new();
    let mut type_functions = HashMap::new();
    builtin::BuiltinFunction::add_builtin_function_headers(&mut functions);

    let mut builtin_functions = HashMap::new();
    for (builtin_fn, _) in &functions {
        builtin_functions.insert(builtin_fn.name.clone(), builtin_fn.clone());
    }

    let mut imports = HashMap::new();

    for (module_id, module) in &program.module_tree {
        let module_imports = module.imports.clone();
        imports.insert(module_id.clone(), module_imports);
    }

    extract_module_functions(
        resolved_types,
        &mut functions,
        &mut type_functions,
        &program.module_tree,
    )?;

    Ok(ResolvedFunctions {
        functions,
        type_functions,
        builtin_functions,
        imports,
    })
}

fn extract_module_functions(
    resolved_types: &ResolvedTypes,
    functions: &mut HashMap<ModuleId, ResolvedFunctionHeader>,
    type_functions: &mut HashMap<TypeId, HashMap<String, ModuleId>>,
    module_tree: &HashMap<ModuleIdentifier, ParsedModule>,
) -> MergerResult<()> {
    for module in module_tree.values() {
        for func_def in &module.functions {
            let header = extract_function(resolved_types, func_def, module, None)?;
            let id = header.id.clone();

            if functions.insert(id.clone(), header).is_some() {
                return Err(anyhow::anyhow!(
                    "Duplicate function definition: {}",
                    id.name
                ));
            }
        }
        for type_impl in &module.type_implementations {
            let resolved_type = resolved_types.resolve_type(
                &module.module_path,
                &type_impl.value.impl_type,
                &module.imports,
            )?;
            let mut impl_functions = HashMap::new();
            for func_def in &type_impl.value.functions {
                let header = extract_function(
                    resolved_types,
                    func_def,
                    module,
                    Some(resolved_type.to_string()),
                )?;
                let id = header.id.clone();

                if functions.insert(id.clone(), header).is_some() {
                    return Err(anyhow::anyhow!(
                        "Duplicate member function definition: {}",
                        id.name
                    ));
                }
                impl_functions.insert(func_def.value.function_name.clone(), id);
            }
            if !type_functions.contains_key(&resolved_type) {
                type_functions.insert(resolved_type.clone(), HashMap::new());
            }
            type_functions
                .get_mut(&resolved_type)
                .unwrap()
                .extend(impl_functions);
        }
    }
    Ok(())
}

fn extract_function(
    resolved_types: &ResolvedTypes,
    func_def: &Src<ParsedFunction>,
    module: &ParsedModule,
    name_prefix: Option<String>,
) -> MergerResult<ResolvedFunctionHeader> {
    let func_name = name_prefix
        .map(|prefix| format!("{}@{}", prefix, func_def.value.function_name.clone()))
        .unwrap_or_else(|| func_def.value.function_name.clone());
    let func_id = ModuleId {
        name: func_name,
        module_path: module.module_path.clone(),
    };

    let return_type = resolved_types.resolve_type(
        &module.module_path,
        &func_def.value.return_type,
        &module.imports,
    )?;

    let mut parameter_order = Vec::with_capacity(func_def.value.args.len());
    let mut parameter_types = HashMap::with_capacity(func_def.value.args.len());

    for (arg_type, arg_name) in &func_def.value.args {
        let arg_type =
            resolved_types.resolve_type(&module.module_path, arg_type, &module.imports)?;
        parameter_order.push(arg_name.clone());
        parameter_types.insert(arg_name.clone(), arg_type);
    }

    let header = ResolvedFunctionHeader {
        id: func_id.clone(),
        return_type,
        parameter_order,
        parameter_types,
    };

    Ok(header)
}
