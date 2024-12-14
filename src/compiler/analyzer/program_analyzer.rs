use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::analyzer::iterative_expression_analyzer::analyze_expression;
use crate::compiler::analyzer::return_analyzer::always_calls_return;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::merger::merged_expression::{FunctionId, MergedProgram, TypeId};
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::ParsedExpression;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub struct AnalyzerContext<'a> {
    pub resolved_functions: &'a ResolvedFunctions,
    pub resolved_types: &'a ResolvedTypes,
    pub local_variables: HashMap<String, LocalVariable>,
    pub return_type: &'a TypeId,
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub ty: TypeId,
    pub is_current_scope: bool,
}

pub fn analyze_program(program: &MergedProgram) -> AnalyzerResult<AnalyzedProgram> {
    let mut analyzed_functions = Vec::with_capacity(program.function_bodies.len());

    for (id, body) in &program.function_bodies {
        let analyzed_function = analyze_function(
            id,
            &program.resolved_types,
            &program.resolved_functions,
            body,
        )?;
        analyzed_functions.push(analyzed_function);
    }

    let main_function_header = program
        .resolved_functions
        .functions
        .get(&ItemId {
            item_name: "main".to_string(),
            module_id: ModuleIdentifier { path: Vec::new() },
        })
        .ok_or_else(|| anyhow::anyhow!("Main function not found"))?;

    if main_function_header.return_type != TypeId::Integer(4) {
        return Err(anyhow::anyhow!(
            "Main function must return int, found {}",
            main_function_header.return_type
        ));
    }

    Ok(AnalyzedProgram {
        resolved_types: program.resolved_types.clone(),
        resolved_functions: program.resolved_functions.clone(),
        functions: analyzed_functions,
    })
}

pub fn analyze_function(
    id: &FunctionId,
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    body: &ParsedExpression,
) -> AnalyzerResult<AnalyzedFunction> {
    let header = resolved_functions.get_header(id).ok_or_else(|| {
        anyhow::anyhow!(
            "Function header not found for function {:?} at {}",
            id,
            body.location
        )
    })?;

    let return_type = header.return_type.clone();
    let mut context = AnalyzerContext {
        resolved_types,
        resolved_functions,
        local_variables: HashMap::new(),
        return_type: &return_type,
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
        if analyzed_body.ty != TypeId::Unit || !always_calls_return(&analyzed_body) {
            return Err(anyhow::anyhow!(
                "All code paths in function body must return {:?}, found {:?} at {}",
                return_type,
                analyzed_body.ty,
                body.location
            ));
        }
    }

    Ok(AnalyzedFunction {
        name: id.clone(),
        body: analyzed_body,
    })
}
