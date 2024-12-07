use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::analyzer::expression_analyzer::analyze_expression;
use crate::compiler::analyzer::return_analyzer::always_calls_return;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::{
    MergedFunction, MergedProgram, ModuleId, ResolvedFunctions, ResolvedStruct, ResolvedTypes,
    TypeId,
};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub struct AnalyzerContext<'a> {
    pub function_headers: &'a ResolvedFunctions,
    pub loop_data: Option<LoopData>,
    pub local_variables: HashMap<String, LocalVariable>,
    pub structs: &'a HashMap<TypeId, ResolvedStruct>,
    pub return_type: &'a TypeId,
}

#[derive(Debug, Clone)]
pub struct LoopData {
    pub break_return_type: TypeId,
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub ty: TypeId,
    pub is_current_scope: bool,
}

pub fn analyze_program(program: &MergedProgram) -> AnalyzerResult<AnalyzedProgram> {
    let mut analyzed_functions = Vec::with_capacity(program.functions.len());

    for (id, function) in &program.functions {
        let analyzed_function = analyze_function(
            id,
            &program.resolved_types,
            &program.resolved_functions,
            function,
        )?;
        analyzed_functions.push(analyzed_function);
    }

    let main_function_header = program
        .resolved_functions
        .functions
        .get(&ModuleId {
            name: "main".to_string(),
            module_path: ModuleIdentifier {
                path: Vec::new(),
                absolute: true,
            },
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
    id: &ModuleId,
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    function: &Src<MergedFunction>,
) -> AnalyzerResult<AnalyzedFunction> {
    let header = resolved_functions
        .functions
        .get(id)
        .ok_or_else(|| anyhow::anyhow!("Function not found: {}", id.name))?;

    let return_type = header.return_type.clone();
    let mut context = AnalyzerContext {
        function_headers: resolved_functions,
        structs: &resolved_types.structs,
        loop_data: None,
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

    let analyzed_body = analyze_expression(&mut context, &function.value.body)?;

    if analyzed_body.ty != return_type {
        if analyzed_body.ty != TypeId::Unit || !always_calls_return(&analyzed_body) {
            return Err(anyhow::anyhow!(
                "All code paths in function body must return {:?}, found {:?} at {}",
                return_type,
                analyzed_body.ty,
                function.location
            ));
        }
    }

    Ok(AnalyzedFunction {
        name: id.clone(),
        body: analyzed_body,
    })
}
