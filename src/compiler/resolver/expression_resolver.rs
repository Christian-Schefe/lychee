use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedConstant, AnalyzedExpression, AnalyzedExpressionKind, AnalyzedLiteral, AnalyzedUnaryOp,
    AssignableExpression, AssignableExpressionKind,
};
use crate::compiler::analyzer::type_resolver::AnalyzedType;
use crate::compiler::resolver::program_resolver::ResolverContext;
use crate::compiler::resolver::resolved_expression::{
    ResolvedAssignableExpression, ResolvedExpression, ResolvedExpressionKind, ResolvedLiteral,
    ResolvedUnaryOp, ValueData,
};

pub fn resolve_expression(
    context: &mut ResolverContext,
    expression: &AnalyzedExpression,
    should_discard: bool,
) -> ResolvedExpression {
    let value_data = ValueData::from_type(&expression.ty, context);
    let stack_discard = value_data.discard_stack_size(should_discard);

    match &expression.kind {
        AnalyzedExpressionKind::Block {
            expressions,
            returns_value,
        } => {
            let old_local_vars = context.local_vars.clone();
            let old_current_local_var_stack_size = context.current_local_var_stack_size;

            let mut resolved_expressions = Vec::with_capacity(expressions.len());
            for (i, expr) in expressions.iter().enumerate() {
                let inner_should_discard =
                    should_discard || !*returns_value || (i + 1 != expressions.len());
                resolved_expressions.push(resolve_expression(context, expr, inner_should_discard));
            }

            context.local_vars = old_local_vars;
            context.current_local_var_stack_size = old_current_local_var_stack_size;

            ResolvedExpression {
                kind: ResolvedExpressionKind::Block(resolved_expressions),
                value_data,
                stack_discard,
            }
        }
        AnalyzedExpressionKind::Return(expr) => {
            if let Some(expr) = expr {
                let resolved_expr = resolve_expression(context, expr, false);
                ResolvedExpression {
                    kind: ResolvedExpressionKind::Return(Some(Box::new(resolved_expr))),
                    stack_discard,
                    value_data,
                }
            } else {
                ResolvedExpression {
                    kind: ResolvedExpressionKind::Return(None),
                    stack_discard,
                    value_data,
                }
            }
        }
        AnalyzedExpressionKind::Continue => ResolvedExpression {
            kind: ResolvedExpressionKind::Continue,
            stack_discard,
            value_data,
        },
        AnalyzedExpressionKind::Break(expr) => {
            if let Some(expr) = expr {
                let resolved_expr = resolve_expression(context, expr, false);
                ResolvedExpression {
                    kind: ResolvedExpressionKind::Break {
                        maybe_expr: Some(Box::new(resolved_expr)),
                    },
                    stack_discard,
                    value_data,
                }
            } else {
                ResolvedExpression {
                    kind: ResolvedExpressionKind::Break { maybe_expr: None },
                    stack_discard,
                    value_data,
                }
            }
        }
        AnalyzedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            let resolved_condition = resolve_expression(context, condition, false);
            let resolved_then_block = resolve_expression(context, then_block, false);
            let resolved_else_expr = else_expr
                .as_ref()
                .map(|expr| resolve_expression(context, expr, false));

            ResolvedExpression {
                kind: ResolvedExpressionKind::If {
                    condition: Box::new(resolved_condition),
                    then_block: Box::new(resolved_then_block),
                    else_expr: resolved_else_expr.map(Box::new),
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::While {
            condition,
            loop_body,
            else_expr,
        } => {
            let resolved_condition = resolve_expression(context, condition, false);
            let resolved_loop_body = resolve_expression(context, loop_body, false);
            let resolved_else_expr = else_expr
                .as_ref()
                .map(|expr| resolve_expression(context, expr, false));

            ResolvedExpression {
                kind: ResolvedExpressionKind::While {
                    condition: Box::new(resolved_condition),
                    loop_body: Box::new(resolved_loop_body),
                    else_expr: resolved_else_expr.map(Box::new),
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::For {
            init,
            condition,
            step,
            loop_body,
            else_expr,
        } => {
            let resolved_init = resolve_expression(context, init, false);
            let resolved_condition = resolve_expression(context, condition, false);
            let resolved_step = resolve_expression(context, step, false);
            let resolved_loop_body = resolve_expression(context, loop_body, false);
            let resolved_else_expr = else_expr
                .as_ref()
                .map(|expr| resolve_expression(context, expr, false));

            ResolvedExpression {
                kind: ResolvedExpressionKind::For {
                    init: Box::new(resolved_init),
                    condition: Box::new(resolved_condition),
                    step: Box::new(resolved_step),
                    loop_body: Box::new(resolved_loop_body),
                    else_expr: resolved_else_expr.map(Box::new),
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Declaration { var_name, value } => {
            let resolved_value = resolve_expression(context, value, false);
            let var_offset =
                context.add_local_var(var_name.clone(), resolved_value.value_data.size);

            ResolvedExpression {
                kind: ResolvedExpressionKind::Declaration {
                    var_offset,
                    value: Box::new(resolved_value),
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::ValueOfAssignable(assignable) => {
            let resolved_assignable = resolve_assignable_expression(context, assignable);

            ResolvedExpression {
                kind: ResolvedExpressionKind::ValueOfAssignable(resolved_assignable),
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Literal(lit) => {
            let kind = match lit {
                AnalyzedLiteral::Unit => ResolvedLiteral::Unit,
                AnalyzedLiteral::Bool(b) => ResolvedLiteral::Bool(*b),
                AnalyzedLiteral::Char(c) => ResolvedLiteral::Char(*c),
                AnalyzedLiteral::Integer(i) => ResolvedLiteral::Integer(*i),
                AnalyzedLiteral::Struct(fields) => {
                    let resolved_fields = fields
                        .iter()
                        .map(|(_, expr)| resolve_expression(context, expr, false))
                        .collect();
                    ResolvedLiteral::Struct(resolved_fields)
                }
            };

            ResolvedExpression {
                kind: ResolvedExpressionKind::Literal(kind),
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Unary { op, expr } => {
            let resolved_expr = resolve_expression(context, expr, false);
            let mapped_op = match op {
                AnalyzedUnaryOp::Math(math_op) => ResolvedUnaryOp::Math(math_op.clone()),
                AnalyzedUnaryOp::LogicalNot => ResolvedUnaryOp::LogicalNot,
                AnalyzedUnaryOp::Cast => match &expression.ty {
                    AnalyzedType::Integer(_) => ResolvedUnaryOp::IntCast,
                    AnalyzedType::Bool => ResolvedUnaryOp::BoolCast,
                    AnalyzedType::Char => ResolvedUnaryOp::IntCast,
                    AnalyzedType::Pointer(_) => ResolvedUnaryOp::PointerCast,
                    _ => {
                        panic!("Unsupported cast: {:?}", expression.ty)
                    }
                },
            };

            ResolvedExpression {
                kind: ResolvedExpressionKind::Unary {
                    op: mapped_op,
                    expr: Box::new(resolved_expr),
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Binary { op, left, right } => {
            let resolved_left = resolve_expression(context, left, false);
            let resolved_right = resolve_expression(context, right, false);

            ResolvedExpression {
                kind: ResolvedExpressionKind::Binary {
                    op: op.clone(),
                    left: Box::new(resolved_left),
                    right: Box::new(resolved_right),
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Assign { op, lhs, rhs } => {
            let resolved_lhs = resolve_assignable_expression(context, lhs);
            let resolved_rhs = resolve_expression(context, rhs, false);

            ResolvedExpression {
                kind: ResolvedExpressionKind::Assign {
                    op: op.clone(),
                    lhs: resolved_lhs,
                    rhs: Box::new(resolved_rhs),
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Borrow { expr } => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Borrow {
                    expr: resolved_expr,
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::FunctionCall {
            function_name,
            args,
        } => {
            let mut arg_stack_size = 0;
            let resolved_args = args
                .iter()
                .map(|expr| {
                    let resolved_expr = resolve_expression(context, expr, false);
                    let arg_size = resolved_expr.value_data.size;
                    arg_stack_size += arg_size;
                    resolved_expr
                })
                .collect();

            let total_stack_discard = stack_discard + arg_stack_size;
            let return_stack_space = value_data.discard_stack_size(true);

            ResolvedExpression {
                kind: ResolvedExpressionKind::FunctionCall {
                    function_name: function_name.clone(),
                    args: resolved_args,
                    return_stack_space,
                },
                stack_discard: total_stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::FieldAccess { expr, field_name } => {
            let resolved_expr = resolve_expression(context, expr, false);
            let field_offset = field_offset(context, &expr.ty, field_name);
            let struct_size = resolved_expr.value_data.size;

            ResolvedExpression {
                kind: ResolvedExpressionKind::FieldAccess {
                    expr: Box::new(resolved_expr),
                    field_offset,
                    struct_size,
                },
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Increment(expr, is_prefix) => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Increment(resolved_expr, *is_prefix),
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::Decrement(expr, is_prefix) => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Decrement(resolved_expr, *is_prefix),
                stack_discard,
                value_data,
            }
        }
        AnalyzedExpressionKind::ConstantPointer(constant) => match constant {
            AnalyzedConstant::String(bytes) => {
                let index = context.constants.len();
                context.constants.push(bytes.clone());
                ResolvedExpression {
                    kind: ResolvedExpressionKind::ConstantPointer(index),
                    stack_discard,
                    value_data,
                }
            }
        },
    }
}

fn resolve_assignable_expression(
    context: &mut ResolverContext,
    expr: &AssignableExpression,
) -> ResolvedAssignableExpression {
    match &expr.kind {
        AssignableExpressionKind::LocalVariable(name) => {
            let var_offset = *context.local_vars.get(name).unwrap();
            ResolvedAssignableExpression::LocalVariable(var_offset)
        }
        AssignableExpressionKind::Dereference(inner) => {
            let resolved_inner = resolve_expression(context, inner, false);
            ResolvedAssignableExpression::Dereference(Box::new(resolved_inner))
        }
        AssignableExpressionKind::FieldAccess(inner, field_name) => {
            let resolved_inner = resolve_assignable_expression(context, inner);
            let field_offset = field_offset(context, &inner.ty, field_name);
            ResolvedAssignableExpression::FieldAccess(Box::new(resolved_inner), field_offset)
        }
        AssignableExpressionKind::ArrayIndex(arr_expr, index_expr) => {
            let resolved_arr_expr = resolve_expression(context, arr_expr, false);
            let resolved_index_expr = resolve_expression(context, index_expr, false);
            let element_size = array_element_size(context, &arr_expr.ty);
            ResolvedAssignableExpression::ArrayIndex(
                Box::new(resolved_arr_expr),
                Box::new(resolved_index_expr),
                element_size,
            )
        }
        AssignableExpressionKind::PointerFieldAccess(inner, field_name) => {
            let resolved_inner = resolve_expression(context, inner, false);
            let field_offset = field_offset(context, &inner.ty, field_name);
            ResolvedAssignableExpression::PointerFieldAccess(Box::new(resolved_inner), field_offset)
        }
    }
}

pub fn type_size(context: &ResolverContext, ty: &AnalyzedType) -> usize {
    type_size_fn(
        |name| context.resolved_types.struct_types.get(name).unwrap().size,
        ty,
    )
}

pub fn type_size_fn<F>(struct_sizes: F, ty: &AnalyzedType) -> usize
where
    F: Fn(&str) -> usize,
{
    match ty {
        AnalyzedType::Integer(size) => *size,
        AnalyzedType::Bool => 1,
        AnalyzedType::Char => 1,
        AnalyzedType::Unit => 0,
        AnalyzedType::Pointer(_) => 8,
        AnalyzedType::Struct(name) => struct_sizes(name),
    }
}

fn field_offset(context: &ResolverContext, struct_type: &AnalyzedType, field_name: &str) -> usize {
    let struct_name = match struct_type {
        AnalyzedType::Struct(name) => name,
        AnalyzedType::Pointer(inner) => match &**inner {
            AnalyzedType::Struct(name) => name,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
    context
        .resolved_types
        .struct_types
        .get(struct_name)
        .unwrap()
        .field_offsets
        .get(field_name)
        .unwrap()
        .clone()
}

fn array_element_size(context: &ResolverContext, ty: &AnalyzedType) -> usize {
    match ty {
        AnalyzedType::Pointer(inner) => type_size(context, inner),
        _ => unreachable!(),
    }
}
