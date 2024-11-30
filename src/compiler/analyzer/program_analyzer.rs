use std::collections::HashMap;
use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::analyzer::expression_analyzer::analyze_expression;
use crate::compiler::analyzer::return_analyzer::always_calls_return;
use crate::compiler::analyzer::type_resolver::{analyze_types, AnalyzedType, AnalyzedTypes};
use crate::compiler::builtin::add_builtin_function_headers;
use crate::compiler::lexer::location::Src;
use crate::compiler::parser2::parsed_expression::{ParsedFunction, ParsedProgram};

pub struct AnalyzerContext<'a> {
    pub analyzed_types: &'a AnalyzedTypes,
    pub function_headers: &'a HashMap<String, FunctionHeader>,
    pub loop_data: Option<LoopData>,
    pub local_variables: HashMap<String, LocalVariable>,
    pub return_type: &'a AnalyzedType,
}

#[derive(Debug, Clone)]
pub struct FunctionHeader {
    pub return_type: AnalyzedType,
    pub parameters: Vec<AnalyzedType>,
}

#[derive(Debug, Clone)]
pub struct LoopData {
    pub break_return_type: AnalyzedType,
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub ty: AnalyzedType,
    pub is_current_scope: bool,
}


pub fn analyze_program(program: &ParsedProgram) -> AnalyzerResult<AnalyzedProgram> {
    let analyzed_types = analyze_types(&program)?;

    analyzed_types.struct_types.iter().for_each(|(name, struct_type)| {
        println!("Struct: {}, size: {}", name, struct_type.size);
        struct_type.fields.iter().for_each(|(field_name, field_type)| {
            println!("  {}: {:?}", field_name, field_type);
        });
    });

    let mut function_headers = HashMap::new();
    for function in &program.functions {
        let return_type = analyzed_types.resolve_type(&function.value.return_type)?;
        let mut parameters = Vec::with_capacity(function.value.args.len());
        for (arg_type, _) in &function.value.args {
            parameters.push(analyzed_types.resolve_type(arg_type)?);
        }
        if function_headers.insert(function.value.function_name.clone(), FunctionHeader { return_type, parameters }).is_some() {
            return Err(anyhow::anyhow!("Duplicate function definition: {} at {}", function.value.function_name, function.location));
        }
    }

    add_builtin_function_headers(&mut function_headers);

    let mut analyzed_functions = Vec::with_capacity(program.functions.len());
    let mut main_function = None;
    for function in &program.functions {
        let analyzed_function = analyze_function(&analyzed_types, &function_headers, function)?;
        if analyzed_function.name == "main" {
            main_function = Some(analyzed_functions.len());
        }
        analyzed_functions.push(analyzed_function);
    }
    if let Some(main_function) = main_function {
        let main_return_type = &analyzed_functions[main_function].return_type;
        if main_return_type != &AnalyzedType::Integer(4) {
            return Err(anyhow::anyhow!("Main function must return int32, found {:?}", main_return_type));
        }

        Ok(AnalyzedProgram {
            analyzed_types,
            functions: analyzed_functions,
        })
    } else {
        Err(anyhow::anyhow!("No main function found"))
    }
}

pub fn analyze_function(analyzed_types: &AnalyzedTypes, function_headers: &HashMap<String, FunctionHeader>, function: &Src<ParsedFunction>) -> AnalyzerResult<AnalyzedFunction> {
    let return_type = analyzed_types.resolve_type(&function.value.return_type)?;

    let mut context = AnalyzerContext {
        analyzed_types,
        function_headers,
        loop_data: None,
        local_variables: HashMap::new(),
        return_type: &return_type,
    };

    let mut parameters = Vec::with_capacity(function.value.args.len());
    for (arg_type, name) in &function.value.args {
        parameters.push((name.clone(), context.analyzed_types.resolve_type(arg_type)?));
    }

    for (name, ty) in &parameters {
        context.local_variables.insert(name.clone(), LocalVariable { ty: ty.clone(), is_current_scope: true });
    }

    let analyzed_body = analyze_expression(&mut context, &function.value.body)?;

    if analyzed_body.ty != return_type {
        if analyzed_body.ty != AnalyzedType::Unit || !always_calls_return(&analyzed_body) {
            return Err(anyhow::anyhow!("All code paths in function body must return {:?}, found {:?} at {}", return_type, analyzed_body.ty, function.location));
        }
    }

    Ok(AnalyzedFunction {
        name: function.value.function_name.clone(),
        return_type,
        parameters,
        body: analyzed_body,
    })
}