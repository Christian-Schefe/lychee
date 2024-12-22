use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedExpression, AnalyzedExpressionKind, AnalyzedProgram,
};
use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::merger::merged_expression::{FunctionId, FunctionRef, StructId, StructRef};
use crate::compiler::parser::parsed_expression::GenericParams;
use crate::compiler::unwrapper::unwrapped_type::{
    UnwrappedExpression, UnwrappedFunction, UnwrappedStruct, UnwrappedTypeId,
};
use std::collections::HashMap;

pub struct UnwrapperContext {
    pub functions: HashMap<String, UnwrappedFunction>,
    pub structs: HashMap<String, UnwrappedStruct>,
}

pub fn unwrap_function(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    function_ref: &FunctionRef,
) {
    let function = program.functions.get(&function_ref.id).unwrap();
    let header = program
        .resolved_functions
        .get_header(&function_ref.id)
        .unwrap();
    let unwrapped_body = unwrap_expression(context, &function_ref.generic_args, &function.body);

    let unwrapped_function = UnwrappedFunction {
        name: function_ref.id.name.clone(),
        return_type: function_ref.return_type.clone(),
        parameter_types: header.parameter_types.clone(),
        body: unwrapped_body,
        parameter_order: vec![],
    };
}

fn unwrap_type(
    ty: &AnalyzedTypeId,
    generic_params: &GenericParams,
    generic_args: Vec<AnalyzedTypeId>,
) -> UnwrappedTypeId {
    match ty {
        AnalyzedTypeId::Unit => UnwrappedTypeId::Unit,
        AnalyzedTypeId::Bool => UnwrappedTypeId::Bool,
        AnalyzedTypeId::Char => UnwrappedTypeId::Char,
        AnalyzedTypeId::Integer(size) => UnwrappedTypeId::Integer(*size),
        AnalyzedTypeId::Pointer(inner) => {
            UnwrappedTypeId::Pointer(Box::new(unwrap_type(inner, generic_params, generic_args)))
        }
        AnalyzedTypeId::StructType(struct_ref) => {
            let struct_id = struct_ref.to_string();
            let struct_type = UnwrappedTypeId::StructType(struct_id);
            let struct_data = unwrap_struct(struct_ref, generic_params, generic_args);
        }
        AnalyzedTypeId::GenericType(name) => {
            let index = generic_params.order.iter().position(|x| x == name).unwrap();
            unwrap_type(&generic_args[index], generic_params, generic_args)
        }
    }
}

fn unwrap_expression(
    context: &mut UnwrapperContext,
    generic_args: &Vec<AnalyzedTypeId>,
    expression: &AnalyzedExpression,
) -> UnwrappedExpression {
    match &expression.kind {
        AnalyzedExpressionKind::Block { .. } => {}
        AnalyzedExpressionKind::Return(_) => {}
        AnalyzedExpressionKind::Continue => {}
        AnalyzedExpressionKind::Break(_) => {}
        AnalyzedExpressionKind::If { .. } => {}
        AnalyzedExpressionKind::Loop { .. } => {}
        AnalyzedExpressionKind::Declaration { .. } => {}
        AnalyzedExpressionKind::ValueOfAssignable(_) => {}
        AnalyzedExpressionKind::Literal(_) => {}
        AnalyzedExpressionKind::ConstantPointer(_) => {}
        AnalyzedExpressionKind::Unary { .. } => {}
        AnalyzedExpressionKind::Binary { .. } => {}
        AnalyzedExpressionKind::Assign { .. } => {}
        AnalyzedExpressionKind::Borrow { .. } => {}
        AnalyzedExpressionKind::FunctionCall { .. } => {}
        AnalyzedExpressionKind::FieldAccess { .. } => {}
        AnalyzedExpressionKind::Increment(_, _) => {}
        AnalyzedExpressionKind::Decrement(_, _) => {}
    }
}
