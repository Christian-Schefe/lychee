use std::collections::HashMap;
use crate::compiler::analyzer::analyzed_expression::{AnalyzedExpression, AnalyzedExpressionKind, AnalyzedLiteral, AnalyzedUnaryOp, AssignableExpression, AssignableExpressionKind};
use crate::compiler::analyzer::type_resolver::AnalyzedType;
use crate::compiler::resolver::program_resolver::ResolverContext;
use crate::compiler::resolver::resolved_expression::{ResolvedAssignableExpression, ResolvedExpression, ResolvedExpressionKind, ResolvedLiteral, ResolvedUnaryOp, ValueLocation};

pub fn resolve_expression(context: &mut ResolverContext, expression: &AnalyzedExpression) -> ResolvedExpression {
    match &expression.kind {
        AnalyzedExpressionKind::Block { expressions } => {
            let old_local_vars = context.local_vars.clone();
            let old_current_local_var_stack_size = context.current_local_var_stack_size;

            let mut resolved_expressions = Vec::with_capacity(expressions.len());
            for expr in expressions {
                resolved_expressions.push(resolve_expression(context, expr));
            }
            let value_location = ValueLocation::from_type(&expression.ty);

            context.local_vars = old_local_vars;
            context.current_local_var_stack_size = old_current_local_var_stack_size;

            ResolvedExpression {
                kind: ResolvedExpressionKind::Block(resolved_expressions),
                value_location,
            }
        }
        AnalyzedExpressionKind::Return(expr) => {
            if let Some(expr) = expr {
                let resolved_expr = resolve_expression(context, expr);
                ResolvedExpression {
                    value_location: resolved_expr.value_location.clone(),
                    kind: ResolvedExpressionKind::Return(Some(Box::new(resolved_expr))),
                }
            } else {
                ResolvedExpression {
                    value_location: ValueLocation::None,
                    kind: ResolvedExpressionKind::Return(None),
                }
            }
        }
        AnalyzedExpressionKind::Continue => {
            ResolvedExpression {
                kind: ResolvedExpressionKind::Continue,
                value_location: ValueLocation::None,
            }
        }
        AnalyzedExpressionKind::Break(expr) => {
            if let Some(expr) = expr {
                let resolved_expr = resolve_expression(context, expr);
                ResolvedExpression {
                    value_location: resolved_expr.value_location.clone(),
                    kind: ResolvedExpressionKind::Break(Some(Box::new(resolved_expr))),
                }
            } else {
                ResolvedExpression {
                    value_location: ValueLocation::None,
                    kind: ResolvedExpressionKind::Break(None),
                }
            }
        }
        AnalyzedExpressionKind::If { condition, then_block, else_expr } => {
            let resolved_condition = resolve_expression(context, condition);
            let resolved_then_block = resolve_expression(context, then_block);
            let resolved_else_expr = else_expr.as_ref().map(|expr| resolve_expression(context, expr));
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::If {
                    condition: Box::new(resolved_condition),
                    then_block: Box::new(resolved_then_block),
                    else_expr: resolved_else_expr.map(Box::new),
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::While { condition, loop_body, else_expr } => {
            let resolved_condition = resolve_expression(context, condition);
            let resolved_loop_body = resolve_expression(context, loop_body);
            let resolved_else_expr = else_expr.as_ref().map(|expr| resolve_expression(context, expr));
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::While {
                    condition: Box::new(resolved_condition),
                    loop_body: Box::new(resolved_loop_body),
                    else_expr: resolved_else_expr.map(Box::new),
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::Declaration { var_type, var_name, value } => {
            let resolved_value = resolve_expression(context, value);
            let type_size = type_size(context, var_type);
            let var_offset = context.add_local_var(var_name.clone(), type_size);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Declaration {
                    var_offset,
                    value: Box::new(resolved_value),
                },
                value_location: ValueLocation::None,
            }
        }
        AnalyzedExpressionKind::Variable(name) => {
            let var_offset = *context.local_vars.get(name).unwrap();
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Variable(var_offset),
                value_location,
            }
        }
        AnalyzedExpressionKind::Literal(lit) => {
            let value_location = ValueLocation::from_type(&expression.ty);
            let kind = match lit {
                AnalyzedLiteral::Unit => ResolvedLiteral::Unit,
                AnalyzedLiteral::Bool(b) => ResolvedLiteral::Bool(*b),
                AnalyzedLiteral::Char(c) => ResolvedLiteral::Char(*c),
                AnalyzedLiteral::Integer(i) => ResolvedLiteral::Integer(*i),
                AnalyzedLiteral::Struct(fields) => {
                    let resolved_fields = fields.iter().map(|(_, expr)| resolve_expression(context, expr)).collect();
                    ResolvedLiteral::Struct(resolved_fields)
                }
                AnalyzedLiteral::Array(values) => {
                    let resolved_values = values.iter().map(|expr| resolve_expression(context, expr)).collect();
                    ResolvedLiteral::Array(resolved_values)
                }
            };
            ResolvedExpression {
                kind: ResolvedExpressionKind::Literal(kind),
                value_location,
            }
        }
        AnalyzedExpressionKind::Unary { op, expr } => {
            let resolved_expr = resolve_expression(context, expr);
            let value_location = ValueLocation::from_type(&expression.ty);
            let mapped_op = match op {
                AnalyzedUnaryOp::Math(math_op) => ResolvedUnaryOp::Math(math_op.clone()),
                AnalyzedUnaryOp::LogicalNot => ResolvedUnaryOp::LogicalNot,
                AnalyzedUnaryOp::Dereference => ResolvedUnaryOp::Dereference,
                AnalyzedUnaryOp::Cast => {
                    let operand_size = type_size(context, &expr.ty);
                    let result_size = type_size(context, &expression.ty);
                    match &expression.ty {
                        AnalyzedType::Integer(_) => ResolvedUnaryOp::IntCast { from: operand_size, to: result_size },
                        AnalyzedType::Bool => ResolvedUnaryOp::BoolCast,
                        AnalyzedType::Char => ResolvedUnaryOp::IntCast { from: operand_size, to: result_size },
                        _ => {
                            println!("Unsupported cast: {:?}", expression.ty);
                            unreachable!()
                        }
                    }
                }
            };
            ResolvedExpression {
                kind: ResolvedExpressionKind::Unary {
                    op: mapped_op,
                    expr: Box::new(resolved_expr),
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::Binary { op, left, right } => {
            let resolved_left = resolve_expression(context, left);
            let resolved_right = resolve_expression(context, right);
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Binary {
                    op: op.clone(),
                    left: Box::new(resolved_left),
                    right: Box::new(resolved_right),
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::Assign { op, lhs, rhs } => {
            let resolved_lhs = resolve_assignable_expression(context, lhs);
            let resolved_rhs = resolve_expression(context, rhs);
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Assign {
                    op: op.clone(),
                    lhs: resolved_lhs,
                    rhs: Box::new(resolved_rhs),
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::Borrow { expr } => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Borrow { expr: resolved_expr },
                value_location,
            }
        }
        AnalyzedExpressionKind::FunctionCall { function_name, args } => {
            let resolved_args = args.iter().map(|expr| resolve_expression(context, expr)).collect();
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::FunctionCall {
                    function_name: function_name.clone(),
                    args: resolved_args,
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::FieldAccess { expr, field_name } => {
            let resolved_expr = resolve_expression(context, expr);
            let field_offset = field_offset(context, &expr.ty, field_name);
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::FieldAccess {
                    expr: Box::new(resolved_expr),
                    field_offset,
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::ArrayIndex { array, index } => {
            let resolved_array = resolve_expression(context, array);
            let resolved_index = resolve_expression(context, index);
            let element_size = array_element_size(context, &array.ty);
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::ArrayIndex {
                    array: Box::new(resolved_array),
                    index: Box::new(resolved_index),
                    element_size,
                },
                value_location,
            }
        }
        AnalyzedExpressionKind::Increment(expr, is_prefix) => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Increment(resolved_expr, *is_prefix),
                value_location,
            }
        }
        AnalyzedExpressionKind::Decrement(expr, is_prefix) => {
            let resolved_expr = resolve_assignable_expression(context, expr);
            let value_location = ValueLocation::from_type(&expression.ty);
            ResolvedExpression {
                kind: ResolvedExpressionKind::Decrement(resolved_expr, *is_prefix),
                value_location,
            }
        }
    }
}

fn resolve_assignable_expression(context: &mut ResolverContext, expr: &AssignableExpression) -> ResolvedAssignableExpression {
    match &expr.kind {
        AssignableExpressionKind::LocalVariable(name) => {
            let var_offset = *context.local_vars.get(name).unwrap();
            ResolvedAssignableExpression::LocalVariable(var_offset)
        }
        AssignableExpressionKind::Dereference(inner) => {
            let resolved_inner = resolve_expression(context, inner);
            ResolvedAssignableExpression::Dereference(Box::new(resolved_inner))
        }
        AssignableExpressionKind::FieldAccess(inner, field_name) => {
            let resolved_inner = resolve_assignable_expression(context, inner);
            let field_offset = field_offset(context, &inner.ty, field_name);
            ResolvedAssignableExpression::FieldAccess(Box::new(resolved_inner), field_offset)
        }
        AssignableExpressionKind::ArrayIndex(arr_expr, index_expr) => {
            let resolved_arr_expr = resolve_assignable_expression(context, arr_expr);
            let resolved_index_expr = resolve_expression(context, index_expr);
            let element_size = array_element_size(context, &arr_expr.ty);
            ResolvedAssignableExpression::ArrayIndex(Box::new(resolved_arr_expr), Box::new(resolved_index_expr), element_size)
        }
    }
}

pub fn type_size(context: &ResolverContext, ty: &AnalyzedType) -> usize {
    type_size_fn(|name| context.resolved_types.struct_types.get(name).unwrap().size, ty)
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
        AnalyzedType::Array(_) => 8,
        AnalyzedType::Struct(name) => struct_sizes(name),
    }
}

fn field_offset(context: &ResolverContext, struct_type: &AnalyzedType, field_name: &str) -> usize {
    let struct_name = match struct_type {
        AnalyzedType::Struct(name) => name,
        _ => unreachable!(),
    };
    context.resolved_types.struct_types.get(struct_name).unwrap().field_offsets.get(field_name).unwrap().clone()
}

fn array_element_size(context: &ResolverContext, ty: &AnalyzedType) -> usize {
    match ty {
        AnalyzedType::Array(inner) => type_size(context, inner),
        _ => unreachable!(),
    }
}