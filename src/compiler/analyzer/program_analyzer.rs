use std::collections::HashMap;
use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::analyzer::expression_analyzer::analyze_expression;
use crate::compiler::analyzer::type_resolver::{analyze_types, AnalyzedType, AnalyzedTypes};
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

fn add_builtin_function_headers(function_headers: &mut HashMap<String, FunctionHeader>) {
    function_headers.insert("readchar".to_string(), FunctionHeader {
        return_type: AnalyzedType::Char,
        parameters: vec![],
    });
    function_headers.insert("writechar".to_string(), FunctionHeader {
        return_type: AnalyzedType::Unit,
        parameters: vec![AnalyzedType::Char],
    });
}

pub fn analyze_program(program: ParsedProgram) -> AnalyzedProgram {
    let analyzed_types = analyze_types(&program).unwrap();

    analyzed_types.struct_types.iter().for_each(|(name, struct_type)| {
        println!("Struct: {}, size: {}", name, struct_type.size);
        struct_type.fields.iter().for_each(|(field_name, field_type)| {
            println!("  {}: {:?}", field_name, field_type);
        });
    });

    let mut function_headers = program.functions.iter().map(|function| {
        let return_type = analyzed_types.resolve_type(&function.value.return_type).unwrap();
        let parameters = function.value.args.iter().map(|(arg_type, _)| {
            analyzed_types.resolve_type(arg_type).unwrap()
        }).collect();
        (function.value.function_name.clone(), FunctionHeader { return_type, parameters })
    }).collect();

    add_builtin_function_headers(&mut function_headers);

    let mut analyzed_functions = Vec::with_capacity(program.functions.len());
    for function in program.functions {
        let analyzed_function = analyze_function(&analyzed_types, &function_headers, function).unwrap();
        analyzed_functions.push(analyzed_function);
    }

    AnalyzedProgram {
        analyzed_types,
    }
}

pub fn analyze_function(analyzed_types: &AnalyzedTypes, function_headers: &HashMap<String, FunctionHeader>, function: Src<ParsedFunction>) -> AnalyzerResult<AnalyzedFunction> {
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

    Ok(AnalyzedFunction {
        name: function.value.function_name,
        return_type,
        parameters,
        body: analyzed_body,
    })
}