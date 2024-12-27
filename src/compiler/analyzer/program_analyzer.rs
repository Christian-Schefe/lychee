use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::analyzer::analyzed_type::{AnalyzedTypeId, GenericParams};
use crate::compiler::analyzer::iterative_expression_analyzer::analyze_expression;
use crate::compiler::analyzer::return_analyzer::always_calls_return;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::merger::merged_expression::{FunctionId, FunctionRef, MergedProgram};
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::ParsedExpression;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub struct AnalyzerContext<'a> {
    pub functions: &'a ResolvedFunctions,
    pub types: &'a ResolvedTypes,
    pub local_variables: HashMap<String, LocalVariable>,
    pub return_type: &'a AnalyzedTypeId,
    pub generic_params: &'a GenericParams,
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub ty: AnalyzedTypeId,
    pub is_current_scope: bool,
}

pub fn analyze_program(program: &MergedProgram) -> AnalyzerResult<AnalyzedProgram> {
    let mut analyzed_function_vec =
        HashMap::with_capacity(program.resolved_functions.function_bodies.len());

    let main_item_id = ItemId {
        item_name: "main".to_string(),
        module_id: ModuleIdentifier { path: Vec::new() },
    };
    let mut main_func_id = None;

    for (id, body) in &program.resolved_functions.function_bodies {
        let analyzed_function = analyze_function(
            id,
            &program.resolved_types,
            &program.resolved_functions,
            body,
        )?;
        analyzed_function_vec.insert(id.clone(), analyzed_function);
        if id.id == main_item_id && id.generic_count == 0 && id.param_count == 0 {
            main_func_id = Some(id.clone());
        }
    }

    let main_func_ref = FunctionRef {
        id: main_func_id.ok_or_else(|| anyhow::anyhow!("Main function not found"))?,
        generic_args: Vec::new(),
        arg_types: Vec::new(),
    };

    let main_function_header = analyzed_function_vec
        .get(&main_func_ref.id)
        .ok_or_else(|| anyhow::anyhow!("Main function not found"))?;

    if main_function_header.return_type != AnalyzedTypeId::Integer(4) {
        return Err(anyhow::anyhow!(
            "Main function must return int, found {}",
            main_function_header.return_type
        ));
    }

    Ok(AnalyzedProgram {
        resolved_types: program.resolved_types.clone(),
        resolved_functions: program.resolved_functions.clone(),
        functions: analyzed_function_vec,
        main_function: main_func_ref,
    })
}

pub fn analyze_function(
    id: &FunctionId,
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    body: &ParsedExpression,
) -> AnalyzerResult<AnalyzedFunction> {
    let header = resolved_functions.get_header(id);

    let return_type = header.return_type.clone();

    let mut context = AnalyzerContext {
        types: resolved_types,
        functions: resolved_functions,
        local_variables: HashMap::new(),
        return_type: &return_type,
        generic_params: &header.generic_params,
    };

    for (name, ty) in &header.parameter_types {
        context.local_variables.insert(
            name.clone(),
            LocalVariable {
                ty: ty.clone(),
                is_current_scope: true,
            },
        );
    }

    let analyzed_body = analyze_expression(&mut context, body)?;

    if analyzed_body.ty != return_type {
        if analyzed_body.ty != AnalyzedTypeId::Unit || !always_calls_return(&analyzed_body) {
            return Err(anyhow::anyhow!(
                "All code paths in function body must return {}, found {} at {}",
                return_type,
                analyzed_body.ty,
                body.location
            ));
        }
    }

    Ok(AnalyzedFunction {
        body: analyzed_body,
        return_type,
    })
}
