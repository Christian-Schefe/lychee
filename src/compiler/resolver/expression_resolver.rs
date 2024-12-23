use crate::compiler::analyzer::analyzed_expression::{AnalyzedConstant, AnalyzedUnaryOp};
use crate::compiler::resolver::program_resolver::ResolverContext;
use crate::compiler::resolver::resolved_expression::{
    ResolvedAssignableExpression, ResolvedExpression, ResolvedExpressionKind, ResolvedLiteral,
    ResolvedUnaryOp, ValueData,
};
use crate::compiler::unwrapper::unwrapped_type::{
    AssignableUnwrappedExpression, AssignableUnwrappedExpressionKind, UnwrappedExpression,
    UnwrappedExpressionKind, UnwrappedLiteral, UnwrappedTypeId,
};

pub fn resolve_expression(
    context: &mut ResolverContext,
    expression: &UnwrappedExpression,
    should_discard: bool,
) -> ResolvedExpression {
    let value_data = ValueData::from_type(&expression.ty, context);
    let stack_discard = value_data.discard_stack_size(should_discard);

    match &expression.kind {
        UnwrappedExpressionKind::Block {
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
        UnwrappedExpressionKind::Return(expr) => {
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
        UnwrappedExpressionKind::Continue => ResolvedExpression {
            kind: ResolvedExpressionKind::Continue,
            stack_discard,
            value_data,
        },
        UnwrappedExpressionKind::Break(expr) => {
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
        UnwrappedExpressionKind::If {
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
        UnwrappedExpressionKind::Loop {
            init,
            condition,
            step,
            loop_body,
            else_expr,
        } => {
            let resolved_init = init
                .as_ref()
                .map(|expr| resolve_expression(context, expr, false));
            let resolved_condition = condition
                .as_ref()
                .map(|expr| resolve_expression(context, expr, false));
            let resolved_step = step
                .as_ref()
                .map(|expr| resolve_expression(context, expr, false));
            let resolved_loop_body = resolve_expression(context, loop_body, false);
            let resolved_else_expr = else_expr
                .as_ref()
                .map(|expr| resolve_expression(context, expr, false));

            ResolvedExpression {
                kind: ResolvedExpressionKind::Loop {
                    init: resolved_init.map(Box::new),
                    condition: resolved_condition.map(Box::new),
                    step: resolved_step.map(Box::new),
                    loop_body: Box::new(resolved_loop_body),
                    else_expr: resolved_else_expr.map(Box::new),
                },
                stack_discard,
                value_data,
            }
        }
        UnwrappedExpressionKind::Declaration { var_name, value } => {
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
        UnwrappedExpressionKind::ValueOfAssignable(assignable) => {
            let resolved_assignable = resolve_assignable_expression(context, assignable);

            ResolvedExpression {
                kind: ResolvedExpressionKind::ValueOfAssignable(resolved_assignable),
                stack_discard,
                value_data,
            }
        }
        UnwrappedExpressionKind::Literal(lit) => {
            let kind = match lit {
                UnwrappedLiteral::Unit => ResolvedLiteral::Unit,
                UnwrappedLiteral::Bool(b) => ResolvedLiteral::Bool(*b),
                UnwrappedLiteral::Char(c) => ResolvedLiteral::Char(*c),
                UnwrappedLiteral::Integer(i) => ResolvedLiteral::Integer(*i),
                UnwrappedLiteral::Struct(fields) => {
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
        UnwrappedExpressionKind::Unary { op, expr } => {
            let resolved_expr = resolve_expression(context, expr, false);
            let mapped_op = match op {
                AnalyzedUnaryOp::Math(math_op) => ResolvedUnaryOp::Math(math_op.clone()),
                AnalyzedUnaryOp::LogicalNot => ResolvedUnaryOp::LogicalNot,
                AnalyzedUnaryOp::Cast => {
                    let original_size = context.get_type_size(&expr.ty);
                    let target_size = context.get_type_size(&expression.ty);
                    let smaller_size = original_size.min(target_size);
                    match &expression.ty {
                        UnwrappedTypeId::Integer(_) => ResolvedUnaryOp::IntCast(smaller_size),
                        UnwrappedTypeId::Bool => ResolvedUnaryOp::BoolCast,
                        UnwrappedTypeId::Char => ResolvedUnaryOp::IntCast(smaller_size),
                        UnwrappedTypeId::Pointer(_) => ResolvedUnaryOp::PointerCast,
                        _ => {
                            panic!("Unsupported cast: {:?}", expression.ty)
                        }
                    }
                }
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
        UnwrappedExpressionKind::Binary { op, left, right } => {
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
        UnwrappedExpressionKind::Assign { op, lhs, rhs } => {
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
        UnwrappedExpressionKind::Borrow { expr } => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Borrow {
                    expr: resolved_expr,
                },
                stack_discard,
                value_data,
            }
        }
        UnwrappedExpressionKind::FunctionCall {
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
        UnwrappedExpressionKind::FieldAccess { expr, field_name } => {
            let resolved_expr = resolve_expression(context, expr, false);
            let field_offset = context.get_field_offset(&expr.ty, field_name);
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
        UnwrappedExpressionKind::Increment(expr, is_prefix) => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Increment(resolved_expr, *is_prefix),
                stack_discard,
                value_data,
            }
        }
        UnwrappedExpressionKind::Decrement(expr, is_prefix) => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Decrement(resolved_expr, *is_prefix),
                stack_discard,
                value_data,
            }
        }
        UnwrappedExpressionKind::ConstantPointer(constant) => match constant {
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
        UnwrappedExpressionKind::Sizeof(ty) => {
            let size = context.get_type_size(ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Literal(ResolvedLiteral::Integer(size as i64)),
                stack_discard,
                value_data,
            }
        }
    }
}

fn resolve_assignable_expression(
    context: &mut ResolverContext,
    expr: &AssignableUnwrappedExpression,
) -> ResolvedAssignableExpression {
    match &expr.kind {
        AssignableUnwrappedExpressionKind::LocalVariable(name) => {
            let var_offset = *context.local_vars.get(name).unwrap_or_else(|| {
                panic!("Local variable not found: {}", name);
            });
            ResolvedAssignableExpression::LocalVariable(var_offset)
        }
        AssignableUnwrappedExpressionKind::Dereference(inner) => {
            let resolved_inner = resolve_expression(context, inner, false);
            ResolvedAssignableExpression::Dereference(Box::new(resolved_inner))
        }
        AssignableUnwrappedExpressionKind::FieldAccess(inner, field_name) => {
            let resolved_inner = resolve_assignable_expression(context, inner);
            let field_offset = context.get_field_offset(&inner.ty, field_name);
            ResolvedAssignableExpression::FieldAccess(Box::new(resolved_inner), field_offset)
        }
        AssignableUnwrappedExpressionKind::ArrayIndex(arr_expr, index_expr) => {
            let resolved_arr_expr = resolve_expression(context, arr_expr, false);
            let resolved_index_expr = resolve_expression(context, index_expr, false);
            let element_size = context.get_type_size(&expr.ty);
            ResolvedAssignableExpression::ArrayIndex(
                Box::new(resolved_arr_expr),
                Box::new(resolved_index_expr),
                element_size,
            )
        }
        AssignableUnwrappedExpressionKind::PointerFieldAccess(inner, field_name, indirections) => {
            let resolved_inner = resolve_expression(context, inner, false);
            let field_offset = context.get_field_offset(&inner.ty, field_name);
            ResolvedAssignableExpression::PointerFieldAccess(
                Box::new(resolved_inner),
                field_offset,
                *indirections,
            )
        }
    }
}
