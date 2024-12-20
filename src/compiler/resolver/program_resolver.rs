use crate::compiler::analyzer::analyzed_expression::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::analyzer::iterative_expression_analyzer::resolve_generic_type;
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::parsed_expression::GenericParams;
use crate::compiler::resolver::expression_resolver::resolve_expression;
use crate::compiler::resolver::resolved_expression::{
    FunctionReturnLocation, ResolvedFunction, ResolvedProgram, ValueData, ValueLocation,
};
use crate::compiler::resolver::struct_resolver::{get_type_size, resolve_structs, ResolvedStructs};
use std::collections::HashMap;

pub struct ResolverContext {
    pub resolved_types: ResolvedTypes,
    pub resolved_functions: ResolvedFunctions,
    pub resolved_structs: ResolvedStructs,
    pub local_vars: HashMap<String, isize>,
    pub maximum_local_var_stack_size: usize,
    pub current_local_var_stack_size: usize,
    pub constants: Vec<Vec<u8>>,
    pub generic_params: Option<GenericParams>,
    pub generic_args: Vec<AnalyzedTypeId>,
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
    pub fn resolve_generic_type(&self, ty: &AnalyzedTypeId) -> AnalyzedTypeId {
        resolve_generic_type(ty, &self.generic_params, &self.generic_args)
    }
    pub fn get_type_size(&self, ty: &AnalyzedTypeId) -> usize {
        let actual_ty = self.resolve_generic_type(ty);
        get_type_size(&actual_ty, &self.resolved_structs.struct_sizes)
    }
    pub fn get_field_offset(&self, ty: &AnalyzedTypeId, field_name: &str) -> usize {
        match ty {
            AnalyzedTypeId::StructType(_, _) => {
                let field_offsets = self.resolved_structs.field_offsets.get(ty).unwrap();
                *field_offsets.get(field_name).unwrap()
            }
            AnalyzedTypeId::Pointer(inner) => self.get_field_offset(inner, field_name),
            _ => unreachable!("Expected struct type"),
        }
    }
}

pub fn resolve_program(program: &AnalyzedProgram) -> ResolvedProgram {
    let mut resolved_functions = Vec::with_capacity(program.functions.len());
    let resolved_structs = resolve_structs(&program.resolved_types, &program.generic_instances);

    let mut context = ResolverContext {
        local_vars: HashMap::new(),
        maximum_local_var_stack_size: 0,
        current_local_var_stack_size: 0,
        resolved_types: program.resolved_types.clone(),
        resolved_functions: program.resolved_functions.clone(),
        constants: Vec::new(),
        generic_params: None,
        generic_args: Vec::new(),
        resolved_structs,
    };

    for function in &program.functions {
        if function.generic_params.is_some() {
            for instance in program
                .generic_instances
                .functions
                .get(&function.name)
                .unwrap_or(&Vec::new())
                .iter()
            {
                let resolved_function = resolve_function(
                    &mut context,
                    function,
                    function.generic_params.clone(),
                    instance.clone(),
                );
                resolved_functions.push(resolved_function);
            }
        } else {
            resolved_functions.push(resolve_function(&mut context, function, None, Vec::new()));
        }
    }
    ResolvedProgram {
        functions: resolved_functions,
        constants: context.constants,
    }
}

fn resolve_function(
    context: &mut ResolverContext,
    function: &AnalyzedFunction,
    generic_params: Option<GenericParams>,
    generic_args: Vec<AnalyzedTypeId>,
) -> ResolvedFunction {
    context.local_vars.clear();
    context.current_local_var_stack_size = 0;
    context.maximum_local_var_stack_size = 0;
    context.generic_params = generic_params;
    context.generic_args = generic_args;

    let header = context
        .resolved_functions
        .get_header(&function.name)
        .expect("Function header not found");

    let mut param_offset = 16;
    for name in header.parameter_order.iter().rev() {
        let ty = header.parameter_types.get(name).unwrap();
        let type_size = context.get_type_size(ty);
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
