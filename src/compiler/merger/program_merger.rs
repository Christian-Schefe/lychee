use crate::compiler::lexer::location::Src;
use crate::compiler::merger::function_resolver::build_resolved_functions;
use crate::compiler::merger::iterative_expression_merger::merge_expression;
use crate::compiler::merger::merged_expression::{
    MergedFunction, MergedProgram, ModuleId, ResolvedFunctions, ResolvedTypes,
};
use crate::compiler::merger::type_resolver::build_resolved_types;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{ParsedFunction, ParsedModule, ParsedProgram};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub fn merge_program(parsed_program: &ParsedProgram) -> MergerResult<MergedProgram> {
    let resolved_types = build_resolved_types(parsed_program)?;
    let resolved_functions = build_resolved_functions(parsed_program, &resolved_types)?;

    let mut functions = HashMap::new();

    for (_, parsed_module) in &parsed_program.module_tree {
        merge_module(
            &mut functions,
            &resolved_types,
            &resolved_functions,
            parsed_module,
        )?;
    }

    Ok(MergedProgram {
        functions,
        resolved_functions,
        resolved_types,
    })
}

pub struct MergerContext<'a> {
    pub resolved_types: &'a ResolvedTypes,
    pub resolved_functions: &'a ResolvedFunctions,
    pub module_path: &'a ModuleIdentifier,
    pub imports: &'a HashMap<String, Src<ModuleId>>,
}

pub fn merge_module(
    functions: &mut HashMap<ModuleId, Src<MergedFunction>>,
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    parsed_module: &ParsedModule,
) -> MergerResult<()> {
    let context = MergerContext {
        resolved_types,
        resolved_functions,
        module_path: &parsed_module.module_path,
        imports: &parsed_module.imports,
    };
    for function in &parsed_module.functions {
        let module_id = ModuleId {
            name: function.value.function_name.clone(),
            module_path: parsed_module.module_path.clone(),
        };
        let merged_function = merge_function(&context, function)?;
        functions.insert(
            module_id,
            Src {
                location: function.location.clone(),
                value: merged_function,
            },
        );
    }

    for type_impl in &parsed_module.type_implementations {
        let resolved_type = resolved_types.resolve_type(
            &parsed_module.module_path,
            &type_impl.value.impl_type,
            &parsed_module.imports,
        )?;
        for function in &type_impl.value.functions {
            let name = format!("{}@{}", resolved_type, function.value.function_name);
            let module_id = ModuleId {
                name,
                module_path: parsed_module.module_path.clone(),
            };
            let merged_function = merge_function(&context, function)?;
            functions.insert(
                module_id,
                Src {
                    location: function.location.clone(),
                    value: merged_function,
                },
            );
        }
    }

    validate_imports(&context)?;

    Ok(())
}

pub fn merge_function(
    context: &MergerContext,
    parsed_function: &Src<ParsedFunction>,
) -> MergerResult<MergedFunction> {
    let body = merge_expression(context, &parsed_function.value.body)?;

    Ok(MergedFunction { body })
}

pub fn validate_imports(context: &MergerContext) -> MergerResult<()> {
    for (_, import_id) in context.imports {
        let is_type = context
            .resolved_types
            .known_types
            .get(&import_id.value)
            .is_some();
        let is_function = context
            .resolved_functions
            .functions
            .get(&import_id.value)
            .is_some();
        if !is_type && !is_function {
            Err(anyhow::anyhow!(
                "Import '{}' in module '{}' does not resolve to a type or function at {}",
                import_id.value,
                context.module_path.get_identifier(),
                import_id.location
            ))?;
        }
    }

    Ok(())
}
