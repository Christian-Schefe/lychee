use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::merger::merged_expression::{ResolvedFunctions, ResolvedTypes};
use crate::compiler::resolver::expression_resolver::resolve_expression;
use crate::compiler::resolver::resolved_expression::{
    FunctionReturnLocation, ResolvedFunction, ResolvedProgram, ValueData, ValueLocation,
};
use std::collections::HashMap;

pub struct ResolverContext {
    pub resolved_types: ResolvedTypes,
    pub resolved_functions: ResolvedFunctions,
    pub local_vars: HashMap<String, isize>,
    pub maximum_local_var_stack_size: usize,
    pub current_local_var_stack_size: usize,
    pub constants: Vec<Vec<u8>>,
}

impl ResolverContext {
    pub fn add_local_var(&mut self, name: String, size: usize) -> isize {
        self.current_local_var_stack_size += size;
        self.maximum_local_var_stack_size = self
            .maximum_local_var_stack_size
            .max(self.current_local_var_stack_size);

        let var_offset = -(self.current_local_var_stack_size as isize);
        self.local_vars.insert(name, var_offset);
        var_offset
    }
}

pub fn resolve_program(program: &AnalyzedProgram) -> ResolvedProgram {
    let mut resolved_functions = Vec::with_capacity(program.functions.len());

    let mut context = ResolverContext {
        local_vars: HashMap::new(),
        maximum_local_var_stack_size: 0,
        current_local_var_stack_size: 0,
        resolved_types: program.resolved_types.clone(),
        resolved_functions: program.resolved_functions.clone(),
        constants: Vec::new(),
    };

    for function in &program.functions {
        resolved_functions.push(resolve_function(&mut context, function));
    }
    ResolvedProgram {
        functions: resolved_functions,
        constants: context.constants,
    }
}

fn resolve_function(
    context: &mut ResolverContext,
    function: &AnalyzedFunction,
) -> ResolvedFunction {
    context.local_vars.clear();
    context.current_local_var_stack_size = 0;
    context.maximum_local_var_stack_size = 0;

    let header = context
        .resolved_functions
        .functions
        .get(&function.name)
        .expect("Function header not found");

    let mut param_offset = 16;
    for name in header.parameter_order.iter().rev() {
        let ty = header.parameter_types.get(name).unwrap();
        let type_size = context.resolved_types.get_type_size(ty);
        context
            .local_vars
            .insert(name.clone(), param_offset as isize);
        param_offset += type_size;
    }
    let return_type_data = ValueData::from_type(&header.return_type, &context);

    let resolved_body = resolve_expression(context, &function.body, false);

    let return_location = match return_type_data.location {
        ValueLocation::Stack => FunctionReturnLocation::Stack {
            offset: param_offset as isize,
            size: return_type_data.size,
        },
        ValueLocation::Register => FunctionReturnLocation::Register,
        ValueLocation::None => FunctionReturnLocation::None,
    };

    ResolvedFunction {
        name: function.name.to_string(),
        value_location: return_location,
        body: resolved_body,
        local_var_stack_size: context.maximum_local_var_stack_size,
    }
}
