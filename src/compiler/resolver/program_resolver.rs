use crate::compiler::resolver::expression_resolver::resolve_expression;
use crate::compiler::resolver::resolved_expression::{
    FunctionReturnLocation, ResolvedFunction, ResolvedProgram, ValueData, ValueLocation,
};
use crate::compiler::resolver::struct_resolver::{
    get_type_size, resolve_structs, StructInformation,
};
use crate::compiler::unwrapper::unwrapped_type::{
    UnwrappedFunction, UnwrappedProgram, UnwrappedTypeId,
};
use std::collections::HashMap;

pub struct ResolverContext {
    pub resolved_structs: StructInformation,
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
    pub fn get_type_size(&self, ty: &UnwrappedTypeId) -> usize {
        get_type_size(&ty, &self.resolved_structs.struct_sizes)
    }
    pub fn get_field_offset(&self, ty: &UnwrappedTypeId, field_name: &str) -> usize {
        match ty {
            UnwrappedTypeId::StructType(id) => {
                let field_offsets =
                    self.resolved_structs
                        .field_offsets
                        .get(id)
                        .unwrap_or_else(|| {
                            panic!("Field offsets not found for struct {:?}", id);
                        });
                *field_offsets.get(field_name).unwrap()
            }
            UnwrappedTypeId::Pointer(inner) => self.get_field_offset(&inner, field_name),
            _ => unreachable!("Expected struct type"),
        }
    }
}

pub fn resolve_program(program: &UnwrappedProgram) -> ResolvedProgram {
    let mut resolved_functions = Vec::with_capacity(program.functions.len());
    let resolved_structs = resolve_structs(&program.structs);

    let mut context = ResolverContext {
        local_vars: HashMap::new(),
        maximum_local_var_stack_size: 0,
        current_local_var_stack_size: 0,
        constants: Vec::new(),
        resolved_structs,
    };

    for (_, function) in &program.functions {
        resolved_functions.push(resolve_function(&mut context, function));
    }

    ResolvedProgram {
        functions: resolved_functions,
        constants: context.constants,
        main_function_name: program.main_function_name.clone(),
    }
}

fn resolve_function(
    context: &mut ResolverContext,
    function: &UnwrappedFunction,
) -> ResolvedFunction {
    context.local_vars.clear();
    context.current_local_var_stack_size = 0;
    context.maximum_local_var_stack_size = 0;

    let mut param_offset = 16;
    for name in function.parameter_order.iter().rev() {
        let ty = function.parameter_types.get(name).unwrap();
        let type_size = context.get_type_size(ty);
        context
            .local_vars
            .insert(name.clone(), param_offset as isize);
        param_offset += type_size;
    }
    let return_type_data = ValueData::from_type(&function.return_type, &context);

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
