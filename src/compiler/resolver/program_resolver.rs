use std::collections::HashMap;
use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::analyzer::type_resolver::{AnalyzedTypes};
use crate::compiler::resolver::expression_resolver::{resolve_expression, type_size};
use crate::compiler::resolver::resolved_expression::{FunctionReturnLocation, ResolvedFunction, ResolvedProgram, ValueData, ValueLocation};
use crate::compiler::resolver::resolved_type::{ResolvedStructType, ResolvedTypes};

pub struct ResolverContext {
    pub resolved_types: ResolvedTypes,
    pub local_vars: HashMap<String, isize>,
    pub maximum_local_var_stack_size: usize,
    pub current_local_var_stack_size: usize,
}

impl ResolverContext {
    pub fn add_local_var(&mut self, name: String, size: usize) -> isize {
        self.current_local_var_stack_size += size;
        self.maximum_local_var_stack_size = self.maximum_local_var_stack_size.max(self.current_local_var_stack_size);

        let var_offset = -(self.current_local_var_stack_size as isize);
        self.local_vars.insert(name, var_offset);
        var_offset
    }
}

pub fn resolve_program(program: &AnalyzedProgram) -> ResolvedProgram {
    let mut resolved_functions = Vec::with_capacity(program.functions.len());

    for function in &program.functions {
        resolved_functions.push(resolve_function(&program.analyzed_types, function));
    }
    ResolvedProgram { functions: resolved_functions }
}

fn resolve_function(analyzed_types: &AnalyzedTypes, function: &AnalyzedFunction) -> ResolvedFunction {
    let mut context = ResolverContext {
        local_vars: HashMap::new(),
        maximum_local_var_stack_size: 0,
        current_local_var_stack_size: 0,
        resolved_types: ResolvedTypes {
            struct_types: HashMap::new(),
        },
    };

    for (name, ty) in &analyzed_types.struct_types {
        let resolved_type = ResolvedStructType::new(analyzed_types, ty);
        context.resolved_types.struct_types.insert(name.clone(), resolved_type);
    }

    let mut param_offset = 16;
    for (name, ty) in function.parameters.iter().rev() {
        let type_size = type_size(&context, ty);
        context.local_vars.insert(name.clone(), param_offset as isize);
        param_offset += type_size;
    }

    let resolved_body = resolve_expression(&mut context, &function.body, false);

    let return_type_data = ValueData::from_type(&function.return_type, &context);
    let return_location = match return_type_data.location {
        ValueLocation::Stack => FunctionReturnLocation::Stack {
            offset: param_offset as isize,
            size: return_type_data.size,
        },
        ValueLocation::Register => FunctionReturnLocation::Register,
        ValueLocation::None => FunctionReturnLocation::None,
    };

    ResolvedFunction {
        name: function.name.clone(),
        value_location: return_location,
        body: resolved_body,
        local_var_stack_size: context.maximum_local_var_stack_size,
    }
}