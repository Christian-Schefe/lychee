use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedExpression, AnalyzedExpressionKind, AnalyzedLiteral, AnalyzedProgram,
    AssignableExpression, AssignableExpressionKind,
};
use crate::compiler::analyzer::analyzed_type::{AnalyzedTypeId, GenericParams};
use crate::compiler::merger::merged_expression::{FunctionRef, StructRef};
use crate::compiler::unwrapper::unwrapped_type::{
    AssignableUnwrappedExpression, AssignableUnwrappedExpressionKind, UnwrappedExpression,
    UnwrappedExpressionKind, UnwrappedFunction, UnwrappedLiteral, UnwrappedStruct, UnwrappedTypeId,
};
use std::collections::{HashMap, HashSet};

pub struct UnwrapperContext {
    pub functions: HashMap<String, UnwrappedFunction>,
    pub structs: HashMap<String, UnwrappedStruct>,
    pub builtin_functions: HashSet<String>,
}

pub fn unwrap_function(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    function_ref: &FunctionRef,
) {
    let function_key = function_ref.to_string();
    if context.builtin_functions.contains(&function_key) {
        return;
    }
    if context.functions.contains_key(&function_key) {
        return;
    }

    let function = program.functions.get(&function_ref.id).unwrap_or_else(|| {
        panic!("Function not found: {}", function_ref.id);
    });
    let header = program
        .resolved_functions
        .get_header(&function_ref.id)
        .unwrap();
    let unwrapped_body = unwrap_expression(
        context,
        program,
        &header.generic_params,
        &function_ref.generic_args,
        &function.body,
    );

    let unwrapped_return_type = unwrap_type(
        context,
        program,
        &header.return_type,
        &header.generic_params,
        &function_ref.generic_args,
    );

    let unwrapped_parameters = header
        .parameter_types
        .iter()
        .map(|(name, ty)| {
            (
                name.clone(),
                unwrap_type(
                    context,
                    program,
                    ty,
                    &header.generic_params,
                    &function_ref.generic_args,
                ),
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
    func_generic_params: &GenericParams,
    func_generic_args: &Vec<AnalyzedTypeId>,
) -> UnwrappedTypeId {
    match ty {
        AnalyzedTypeId::Unit => UnwrappedTypeId::Unit,
        AnalyzedTypeId::Bool => UnwrappedTypeId::Bool,
        AnalyzedTypeId::Char => UnwrappedTypeId::Char,
        AnalyzedTypeId::Integer(size) => UnwrappedTypeId::Integer(*size),
        AnalyzedTypeId::Pointer(inner) => UnwrappedTypeId::Pointer(Box::new(unwrap_type(
            context,
            program,
            inner,
            func_generic_params,
            func_generic_args,
        ))),
        AnalyzedTypeId::StructType(struct_ref) => {
            let struct_id = struct_ref.to_string();
            unwrap_struct(
                context,
                program,
                struct_ref,
                func_generic_params,
                func_generic_args,
                &struct_ref.generic_args,
            );
            UnwrappedTypeId::StructType(struct_id)
        }
        AnalyzedTypeId::GenericType(name) => {
            if let Some(generic) = func_generic_params.resolve(&name, &func_generic_args) {
                return unwrap_type(
                    context,
                    program,
                    &generic,
                    func_generic_params,
                    func_generic_args,
                );
            }
            panic!("Generic type not found: {}", name);
        }
    }
}

fn unwrap_struct(
    context: &mut UnwrapperContext,
    program: &AnalyzedProgram,
    struct_ref: &StructRef,
    function_generic_params: &GenericParams,
    function_generic_args: &Vec<AnalyzedTypeId>,
    struct_generic_args: &Vec<AnalyzedTypeId>,
) {
    let struct_key = struct_ref.to_string();
    if context.structs.contains_key(&struct_key) {
        return;
    }

    let struct_def = program.resolved_types.get_struct(&struct_ref).unwrap();
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
                    &function_generic_params,
                    function_generic_args,
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
    func_generic_params: &GenericParams,
    func_generic_args: &Vec<AnalyzedTypeId>,
    expression: &AnalyzedExpression,
) -> UnwrappedExpression {
    let unwrapped_type = unwrap_type(
        context,
        &program,
        &expression.ty,
        &func_generic_params,
        func_generic_args,
    );

    let kind = match &expression.kind {
        AnalyzedExpressionKind::Block {
            expressions,
            returns_value,
        } => {
            let unwrapped_expressions = expressions
                .iter()
                .map(|expr| {
                    unwrap_expression(
                        context,
                        program,
                        func_generic_params,
                        func_generic_args,
                        expr,
                    )
                })
                .collect();
            UnwrappedExpressionKind::Block {
                expressions: unwrapped_expressions,
                returns_value: *returns_value,
            }
        }
        AnalyzedExpressionKind::Return(inner) => {
            let unwrapped_inner = inner.as_ref().map(|inner| {
                Box::new(unwrap_expression(
                    context,
                    program,
                    func_generic_params,
                    func_generic_args,
                    inner,
                ))
            });
            UnwrappedExpressionKind::Return(unwrapped_inner)
        }
        AnalyzedExpressionKind::Continue => UnwrappedExpressionKind::Continue,
        AnalyzedExpressionKind::Break(inner) => {
            let unwrapped_inner = inner.as_ref().map(|inner| {
                Box::new(unwrap_expression(
                    context,
                    program,
                    func_generic_params,
                    func_generic_args,
                    inner,
                ))
            });
            UnwrappedExpressionKind::Break(unwrapped_inner)
        }
        AnalyzedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            let unwrapped_condition = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                condition,
            );
            let unwrapped_then_block = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                then_block,
            );
            let unwrapped_else_expr = else_expr.as_ref().map(|else_expr| {
                Box::new(unwrap_expression(
                    context,
                    program,
                    func_generic_params,
                    func_generic_args,
                    else_expr,
                ))
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
            let unwrapped_init = init.as_ref().map(|init| {
                Box::new(unwrap_expression(
                    context,
                    program,
                    func_generic_params,
                    func_generic_args,
                    init,
                ))
            });
            let unwrapped_condition = condition.as_ref().map(|condition| {
                Box::new(unwrap_expression(
                    context,
                    program,
                    func_generic_params,
                    func_generic_args,
                    condition,
                ))
            });
            let unwrapped_step = step.as_ref().map(|step| {
                Box::new(unwrap_expression(
                    context,
                    program,
                    func_generic_params,
                    func_generic_args,
                    step,
                ))
            });
            let unwrapped_loop_body = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                loop_body,
            );
            let unwrapped_else_expr = else_expr.as_ref().map(|else_expr| {
                Box::new(unwrap_expression(
                    context,
                    program,
                    func_generic_params,
                    func_generic_args,
                    else_expr,
                ))
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
            let unwrapped_value = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                value,
            );
            UnwrappedExpressionKind::Declaration {
                var_name: var_name.clone(),
                value: Box::new(unwrapped_value),
            }
        }
        AnalyzedExpressionKind::ValueOfAssignable(inner) => {
            let unwrapped_inner = unwrap_assignable_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                inner,
            );
            UnwrappedExpressionKind::ValueOfAssignable(unwrapped_inner)
        }
        AnalyzedExpressionKind::Literal(lit) => match lit {
            AnalyzedLiteral::Unit => UnwrappedExpressionKind::Literal(UnwrappedLiteral::Unit),
            AnalyzedLiteral::Bool(value) => {
                UnwrappedExpressionKind::Literal(UnwrappedLiteral::Bool(*value))
            }
            AnalyzedLiteral::Char(value) => {
                UnwrappedExpressionKind::Literal(UnwrappedLiteral::Char(*value))
            }
            AnalyzedLiteral::Integer(value) => {
                UnwrappedExpressionKind::Literal(UnwrappedLiteral::Integer(*value))
            }
            AnalyzedLiteral::Struct(fields) => {
                let unwrapped_fields = fields
                    .iter()
                    .map(|(name, expr)| {
                        (
                            name.clone(),
                            unwrap_expression(
                                context,
                                program,
                                func_generic_params,
                                func_generic_args,
                                expr,
                            ),
                        )
                    })
                    .collect();
                UnwrappedExpressionKind::Literal(UnwrappedLiteral::Struct(unwrapped_fields))
            }
        },
        AnalyzedExpressionKind::ConstantPointer(constant) => {
            UnwrappedExpressionKind::ConstantPointer(constant.clone())
        }
        AnalyzedExpressionKind::Unary { op, expr } => {
            let unwrapped_expr = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                expr,
            );
            UnwrappedExpressionKind::Unary {
                op: op.clone(),
                expr: Box::new(unwrapped_expr),
            }
        }
        AnalyzedExpressionKind::Binary { op, left, right } => {
            let unwrapped_left = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                left,
            );
            let unwrapped_right = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                right,
            );
            UnwrappedExpressionKind::Binary {
                op: op.clone(),
                left: Box::new(unwrapped_left),
                right: Box::new(unwrapped_right),
            }
        }
        AnalyzedExpressionKind::Assign { op, lhs, rhs } => {
            let unwrapped_lhs = unwrap_assignable_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                lhs,
            );
            let unwrapped_rhs = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                rhs,
            );
            UnwrappedExpressionKind::Assign {
                op: op.clone(),
                lhs: unwrapped_lhs,
                rhs: Box::new(unwrapped_rhs),
            }
        }
        AnalyzedExpressionKind::Borrow { expr } => {
            let unwrapped_expr = unwrap_assignable_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                expr,
            );
            UnwrappedExpressionKind::Borrow {
                expr: unwrapped_expr,
            }
        }
        AnalyzedExpressionKind::FunctionCall {
            function_name,
            args,
        } => {
            unwrap_function(context, program, function_name);
            let unwrapped_args = args
                .iter()
                .map(|arg| {
                    unwrap_expression(
                        context,
                        program,
                        func_generic_params,
                        func_generic_args,
                        arg,
                    )
                })
                .collect();
            UnwrappedExpressionKind::FunctionCall {
                function_name: function_name.to_string(),
                args: unwrapped_args,
            }
        }
        AnalyzedExpressionKind::FieldAccess { expr, field_name } => {
            let unwrapped_expr = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                expr,
            );
            UnwrappedExpressionKind::FieldAccess {
                expr: Box::new(unwrapped_expr),
                field_name: field_name.clone(),
            }
        }
        AnalyzedExpressionKind::Increment(inner, post) => {
            let unwrapped_inner = unwrap_assignable_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                inner,
            );
            UnwrappedExpressionKind::Increment(unwrapped_inner, *post)
        }
        AnalyzedExpressionKind::Decrement(inner, post) => {
            let unwrapped_inner = unwrap_assignable_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                inner,
            );
            UnwrappedExpressionKind::Decrement(unwrapped_inner, *post)
        }
        AnalyzedExpressionKind::Sizeof(ty) => {
            let unwrapped_ty =
                unwrap_type(context, program, ty, func_generic_params, func_generic_args);
            UnwrappedExpressionKind::Sizeof(unwrapped_ty)
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
    func_generic_params: &GenericParams,
    func_generic_args: &Vec<AnalyzedTypeId>,
    assignable: &AssignableExpression,
) -> AssignableUnwrappedExpression {
    let unwrapped_type = unwrap_type(
        context,
        &program,
        &assignable.ty,
        &func_generic_params,
        func_generic_args,
    );

    let kind = match &assignable.kind {
        AssignableExpressionKind::LocalVariable(name) => {
            AssignableUnwrappedExpressionKind::LocalVariable(name.clone())
        }
        AssignableExpressionKind::Dereference(inner) => {
            let unwrapped_inner = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                inner,
            );
            AssignableUnwrappedExpressionKind::Dereference(Box::new(unwrapped_inner))
        }
        AssignableExpressionKind::FieldAccess(inner, field_name) => {
            let unwrapped_inner = unwrap_assignable_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                inner,
            );
            AssignableUnwrappedExpressionKind::FieldAccess(
                Box::new(unwrapped_inner),
                field_name.clone(),
            )
        }
        AssignableExpressionKind::PointerFieldAccess(inner, field_name, indirections) => {
            let unwrapped_inner = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                inner,
            );
            AssignableUnwrappedExpressionKind::PointerFieldAccess(
                Box::new(unwrapped_inner),
                field_name.clone(),
                *indirections,
            )
        }
        AssignableExpressionKind::ArrayIndex(array, index) => {
            let unwrapped_array = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                array,
            );
            let unwrapped_index = unwrap_expression(
                context,
                program,
                func_generic_params,
                func_generic_args,
                index,
            );
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
