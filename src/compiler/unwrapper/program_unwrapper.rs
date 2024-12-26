use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedExpression, AnalyzedExpressionKind, AnalyzedFunctionCallType, AnalyzedProgram,
    AssignableExpression, AssignableExpressionKind,
};
use crate::compiler::analyzer::analyzed_type::{AnalyzedTypeId, GenericParams};
use crate::compiler::merger::merged_expression::{FunctionRef, StructRef};
use crate::compiler::unwrapper::unwrapped_type::{
    AssignableUnwrappedExpression, AssignableUnwrappedExpressionKind, UnwrappedExpression,
    UnwrappedExpressionKind, UnwrappedFunction, UnwrappedFunctionCallType, UnwrappedFunctionRef,
    UnwrappedStruct, UnwrappedStructRef, UnwrappedTypeId,
};
use std::collections::{HashMap, HashSet};

pub struct UnwrapperContext {
    pub functions: HashMap<String, UnwrappedFunction>,
    pub structs: HashMap<String, UnwrappedStruct>,
    pub builtin_functions: HashSet<String>,
}

#[derive(Debug)]
pub struct GenericInfo {
    pub generic_params: GenericParams,
    pub generic_args: Vec<UnwrappedTypeId>,
}

pub fn unwrap_function(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    function_ref: &UnwrappedFunctionRef,
) {
    let function_key = function_ref.get_key();
    if context.builtin_functions.contains(&function_key) {
        return;
    }
    if context.functions.contains_key(&function_key) {
        return;
    }

    let function = program.functions.get(&function_ref.id).unwrap_or_else(|| {
        panic!("Function not found: {}", function_ref.id);
    });
    let header = program.resolved_functions.get_header(&function_ref.id);

    let generic_info = GenericInfo {
        generic_params: header.generic_params.clone(),
        generic_args: function_ref.generic_args.clone(),
    };

    let unwrapped_body = unwrap_expression(context, program, &generic_info, &function.body);

    let unwrapped_return_type = unwrap_type(context, program, &header.return_type, &generic_info);

    let unwrapped_parameters = header
        .parameter_types
        .iter()
        .map(|(name, ty)| {
            (
                name.clone(),
                unwrap_type(context, program, ty, &generic_info),
            )
        })
        .collect();

    let unwrapped_function = UnwrappedFunction {
        name: function_key.clone(),
        return_type: unwrapped_return_type,
        parameter_types: unwrapped_parameters,
        body: unwrapped_body,
        parameter_order: header.parameter_order.clone(),
    };
    context.functions.insert(function_key, unwrapped_function);
}

fn unwrap_type(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    ty: &AnalyzedTypeId,
    generic_info: &GenericInfo,
) -> UnwrappedTypeId {
    match ty {
        AnalyzedTypeId::Unit => UnwrappedTypeId::Unit,
        AnalyzedTypeId::Bool => UnwrappedTypeId::Bool,
        AnalyzedTypeId::Char => UnwrappedTypeId::Char,
        AnalyzedTypeId::Integer(size) => UnwrappedTypeId::Integer(*size),
        AnalyzedTypeId::Pointer(inner) => {
            UnwrappedTypeId::Pointer(Box::new(unwrap_type(context, program, inner, generic_info)))
        }
        AnalyzedTypeId::StructType(struct_ref) => {
            let unwrapped_struct_ref =
                unwrap_struct_ref(context, program, generic_info, struct_ref);
            let struct_id = unwrapped_struct_ref.get_key();
            unwrap_struct(
                context,
                program,
                &unwrapped_struct_ref,
                generic_info,
                &struct_ref.generic_args,
            );
            UnwrappedTypeId::StructType(struct_id)
        }
        AnalyzedTypeId::EnumType(_) => UnwrappedTypeId::Integer(4),
        AnalyzedTypeId::GenericType(name) => {
            if let Some(generic) = generic_info
                .generic_params
                .resolve(&name, &generic_info.generic_args)
            {
                return generic;
            }
            panic!("Generic type not found: {}\n{:?}", name, generic_info);
        }
        AnalyzedTypeId::FunctionType(return_type, params) => {
            let unwrapped_return_type = unwrap_type(context, program, return_type, generic_info);
            let unwrapped_params = params
                .iter()
                .map(|param| unwrap_type(context, program, param, generic_info))
                .collect();
            UnwrappedTypeId::FunctionType(Box::new(unwrapped_return_type), unwrapped_params)
        }
    }
}

fn unwrap_struct(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    struct_ref: &UnwrappedStructRef,
    generic_info: &GenericInfo,
    struct_generic_args: &Vec<AnalyzedTypeId>,
) {
    let struct_key = struct_ref.get_key();
    if context.structs.contains_key(&struct_key) {
        return;
    }

    let struct_def = &struct_ref.struct_def;
    let field_types = struct_def
        .field_order
        .iter()
        .map(|name| {
            (
                name.clone(),
                unwrap_type(
                    context,
                    program,
                    &struct_def
                        .get_field_type(name, &struct_generic_args)
                        .unwrap(),
                    generic_info,
                ),
            )
        })
        .collect();
    let unwrapped_struct = UnwrappedStruct {
        field_types,
        field_order: struct_def.field_order.clone(),
    };
    context.structs.insert(struct_key, unwrapped_struct);
}

fn unwrap_expression(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    generic_info: &GenericInfo,
    expression: &AnalyzedExpression,
) -> UnwrappedExpression {
    let unwrapped_type = unwrap_type(context, &program, &expression.ty, generic_info);

    let kind = match &expression.kind {
        AnalyzedExpressionKind::Block {
            expressions,
            returns_value,
        } => {
            let unwrapped_expressions = expressions
                .iter()
                .map(|expr| unwrap_expression(context, program, generic_info, expr))
                .collect();
            UnwrappedExpressionKind::Block {
                expressions: unwrapped_expressions,
                returns_value: *returns_value,
            }
        }
        AnalyzedExpressionKind::Return(inner) => {
            let unwrapped_inner = inner
                .as_ref()
                .map(|inner| Box::new(unwrap_expression(context, program, generic_info, inner)));
            UnwrappedExpressionKind::Return(unwrapped_inner)
        }
        AnalyzedExpressionKind::Continue => UnwrappedExpressionKind::Continue,
        AnalyzedExpressionKind::Break(inner) => {
            let unwrapped_inner = inner
                .as_ref()
                .map(|inner| Box::new(unwrap_expression(context, program, generic_info, inner)));
            UnwrappedExpressionKind::Break(unwrapped_inner)
        }
        AnalyzedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            let unwrapped_condition = unwrap_expression(context, program, generic_info, condition);
            let unwrapped_then_block =
                unwrap_expression(context, program, generic_info, then_block);
            let unwrapped_else_expr = else_expr.as_ref().map(|else_expr| {
                Box::new(unwrap_expression(context, program, generic_info, else_expr))
            });
            UnwrappedExpressionKind::If {
                condition: Box::new(unwrapped_condition),
                then_block: Box::new(unwrapped_then_block),
                else_expr: unwrapped_else_expr,
            }
        }
        AnalyzedExpressionKind::Loop {
            init,
            condition,
            step,
            loop_body,
            else_expr,
        } => {
            let unwrapped_init = init
                .as_ref()
                .map(|init| Box::new(unwrap_expression(context, program, generic_info, init)));
            let unwrapped_condition = condition.as_ref().map(|condition| {
                Box::new(unwrap_expression(context, program, generic_info, condition))
            });
            let unwrapped_step = step
                .as_ref()
                .map(|step| Box::new(unwrap_expression(context, program, generic_info, step)));
            let unwrapped_loop_body = unwrap_expression(context, program, generic_info, loop_body);
            let unwrapped_else_expr = else_expr.as_ref().map(|else_expr| {
                Box::new(unwrap_expression(context, program, generic_info, else_expr))
            });
            UnwrappedExpressionKind::Loop {
                init: unwrapped_init,
                condition: unwrapped_condition,
                step: unwrapped_step,
                loop_body: Box::new(unwrapped_loop_body),
                else_expr: unwrapped_else_expr,
            }
        }
        AnalyzedExpressionKind::Declaration { var_name, value } => {
            let unwrapped_value = unwrap_expression(context, program, generic_info, value);
            UnwrappedExpressionKind::Declaration {
                var_name: var_name.clone(),
                value: Box::new(unwrapped_value),
            }
        }
        AnalyzedExpressionKind::ValueOfAssignable(inner) => {
            let unwrapped_inner =
                unwrap_assignable_expression(context, program, generic_info, inner);
            UnwrappedExpressionKind::ValueOfAssignable(unwrapped_inner)
        }
        AnalyzedExpressionKind::StructInstance { fields } => {
            let unwrapped_fields = fields
                .iter()
                .map(|(name, expr)| {
                    (
                        name.clone(),
                        unwrap_expression(context, program, generic_info, expr),
                    )
                })
                .collect();
            UnwrappedExpressionKind::StructInstance {
                fields: unwrapped_fields,
            }
        }
        AnalyzedExpressionKind::Literal(lit) => UnwrappedExpressionKind::Literal(lit.clone()),
        AnalyzedExpressionKind::ConstantPointer(constant) => {
            UnwrappedExpressionKind::ConstantPointer(constant.clone())
        }
        AnalyzedExpressionKind::Unary { op, expr } => {
            let unwrapped_expr = unwrap_expression(context, program, generic_info, expr);
            UnwrappedExpressionKind::Unary {
                op: op.clone(),
                expr: Box::new(unwrapped_expr),
            }
        }
        AnalyzedExpressionKind::Binary { op, left, right } => {
            let unwrapped_left = unwrap_expression(context, program, generic_info, left);
            let unwrapped_right = unwrap_expression(context, program, generic_info, right);
            UnwrappedExpressionKind::Binary {
                op: op.clone(),
                left: Box::new(unwrapped_left),
                right: Box::new(unwrapped_right),
            }
        }
        AnalyzedExpressionKind::Assign { op, lhs, rhs } => {
            let unwrapped_lhs = unwrap_assignable_expression(context, program, generic_info, lhs);
            let unwrapped_rhs = unwrap_expression(context, program, generic_info, rhs);
            UnwrappedExpressionKind::Assign {
                op: op.clone(),
                lhs: unwrapped_lhs,
                rhs: Box::new(unwrapped_rhs),
            }
        }
        AnalyzedExpressionKind::Borrow { expr } => {
            let unwrapped_expr = unwrap_assignable_expression(context, program, generic_info, expr);
            UnwrappedExpressionKind::Borrow {
                expr: unwrapped_expr,
            }
        }
        AnalyzedExpressionKind::FunctionCall { call_type, args } => {
            let unwrapped_call_type = match call_type {
                AnalyzedFunctionCallType::FunctionPointer(inner) => {
                    let unwrapped_expr = unwrap_expression(context, program, generic_info, inner);
                    UnwrappedFunctionCallType::Pointer(Box::new(unwrapped_expr))
                }
                AnalyzedFunctionCallType::Function(function) => {
                    let unwrapped_function_ref =
                        unwrap_function_ref(context, program, generic_info, function);
                    unwrap_function(context, program, &unwrapped_function_ref);
                    UnwrappedFunctionCallType::Function(unwrapped_function_ref.get_key())
                }
            };
            let unwrapped_args = args
                .iter()
                .map(|arg| unwrap_expression(context, program, generic_info, arg))
                .collect();
            UnwrappedExpressionKind::FunctionCall {
                call_type: unwrapped_call_type,
                args: unwrapped_args,
            }
        }
        AnalyzedExpressionKind::FieldAccess { expr, field_name } => {
            let unwrapped_expr = unwrap_expression(context, program, generic_info, expr);
            UnwrappedExpressionKind::FieldAccess {
                expr: Box::new(unwrapped_expr),
                field_name: field_name.clone(),
            }
        }
        AnalyzedExpressionKind::Increment(inner, post) => {
            let unwrapped_inner =
                unwrap_assignable_expression(context, program, generic_info, inner);
            UnwrappedExpressionKind::Increment(unwrapped_inner, *post)
        }
        AnalyzedExpressionKind::Decrement(inner, post) => {
            let unwrapped_inner =
                unwrap_assignable_expression(context, program, generic_info, inner);
            UnwrappedExpressionKind::Decrement(unwrapped_inner, *post)
        }
        AnalyzedExpressionKind::Sizeof(ty) => {
            let unwrapped_ty = unwrap_type(context, program, ty, generic_info);
            UnwrappedExpressionKind::Sizeof(unwrapped_ty)
        }
        AnalyzedExpressionKind::FunctionPointer(function) => {
            let unwrapped_function_ref =
                unwrap_function_ref(context, program, generic_info, function);
            unwrap_function(context, program, &unwrapped_function_ref);
            UnwrappedExpressionKind::FunctionPointer(unwrapped_function_ref.get_key())
        }
    };

    UnwrappedExpression {
        kind,
        ty: unwrapped_type,
    }
}

fn unwrap_assignable_expression(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    generic_info: &GenericInfo,
    assignable: &AssignableExpression,
) -> AssignableUnwrappedExpression {
    let unwrapped_type = unwrap_type(context, &program, &assignable.ty, generic_info);

    let kind = match &assignable.kind {
        AssignableExpressionKind::LocalVariable(name) => {
            AssignableUnwrappedExpressionKind::LocalVariable(name.clone())
        }
        AssignableExpressionKind::Dereference(inner) => {
            let unwrapped_inner = unwrap_expression(context, program, generic_info, inner);
            AssignableUnwrappedExpressionKind::Dereference(Box::new(unwrapped_inner))
        }
        AssignableExpressionKind::FieldAccess(inner, field_name) => {
            let unwrapped_inner =
                unwrap_assignable_expression(context, program, generic_info, inner);
            AssignableUnwrappedExpressionKind::FieldAccess(
                Box::new(unwrapped_inner),
                field_name.clone(),
            )
        }
        AssignableExpressionKind::PointerFieldAccess(inner, field_name, indirections) => {
            let unwrapped_inner = unwrap_expression(context, program, generic_info, inner);
            AssignableUnwrappedExpressionKind::PointerFieldAccess(
                Box::new(unwrapped_inner),
                field_name.clone(),
                *indirections,
            )
        }
        AssignableExpressionKind::ArrayIndex(array, index) => {
            let unwrapped_array = unwrap_expression(context, program, generic_info, array);
            let unwrapped_index = unwrap_expression(context, program, generic_info, index);
            AssignableUnwrappedExpressionKind::ArrayIndex(
                Box::new(unwrapped_array),
                Box::new(unwrapped_index),
            )
        }
    };

    AssignableUnwrappedExpression {
        kind,
        ty: unwrapped_type,
    }
}

pub fn unwrap_function_ref(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    generic_info: &GenericInfo,
    function_ref: &FunctionRef,
) -> UnwrappedFunctionRef {
    let mut unwrapped_generic_args = Vec::new();
    let mut unwrapped_arg_types = Vec::new();
    for generic_arg in &function_ref.generic_args {
        unwrapped_generic_args.push(unwrap_type(context, program, generic_arg, generic_info));
    }
    for arg_type in &function_ref.arg_types {
        unwrapped_arg_types.push(unwrap_type(context, program, arg_type, generic_info));
    }
    UnwrappedFunctionRef {
        id: function_ref.id.clone(),
        generic_args: unwrapped_generic_args,
        arg_types: unwrapped_arg_types,
    }
}

pub fn unwrap_struct_ref<'a>(
    context: &mut UnwrapperContext,
    program: &'a AnalyzedProgram,
    generic_info: &GenericInfo,
    struct_ref: &'a StructRef,
) -> UnwrappedStructRef<'a> {
    let mut unwrapped_generic_args = Vec::new();
    for generic_arg in &struct_ref.generic_args {
        unwrapped_generic_args.push(unwrap_type(context, program, generic_arg, generic_info));
    }
    let resolved_struct = program.resolved_types.get_struct(struct_ref).unwrap();
    UnwrappedStructRef {
        id: struct_ref.id.clone(),
        generic_args: unwrapped_generic_args,
        struct_def: resolved_struct,
    }
}
