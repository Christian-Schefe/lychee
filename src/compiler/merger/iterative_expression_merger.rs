use crate::compiler::merger::merged_expression::{
    MergedExpression, MergedExpressionKind, MergedLiteral, MergedUnaryOp,
};
use crate::compiler::merger::program_merger::MergerContext;
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{
    ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryOp,
};
use std::collections::HashMap;

pub fn merge_expression(
    context: &MergerContext,
    parsed_expression: &ParsedExpression,
) -> MergerResult<MergedExpression> {
    let mut stack = vec![(parsed_expression, false)];
    let mut output = vec![];
    while let Some((stack_expr, was_visited)) = stack.pop() {
        let location = stack_expr.location.clone();
        if !was_visited {
            stack.push((stack_expr, true));
            match &stack_expr.value {
                ParsedExpressionKind::Block { expressions, .. } => {
                    for expression in expressions.iter().rev() {
                        stack.push((expression, false));
                    }
                }
                ParsedExpressionKind::Return(maybe_expr) => {
                    if let Some(expr) = maybe_expr {
                        stack.push((expr, false));
                    }
                }
                ParsedExpressionKind::Continue => {}
                ParsedExpressionKind::Break(maybe_expr) => {
                    if let Some(expr) = maybe_expr {
                        stack.push((expr, false));
                    }
                }
                ParsedExpressionKind::If {
                    condition,
                    then_block,
                    else_expr,
                } => {
                    if let Some(expr) = else_expr {
                        stack.push((expr, false));
                    }
                    stack.push((then_block, false));
                    stack.push((condition, false));
                }
                ParsedExpressionKind::Loop {
                    init,
                    condition,
                    step,
                    loop_body,
                    else_expr,
                } => {
                    if let Some(expr) = else_expr {
                        stack.push((expr, false));
                    }
                    stack.push((loop_body, false));
                    if let Some(expr) = step {
                        stack.push((expr, false));
                    }
                    if let Some(expr) = condition {
                        stack.push((expr, false));
                    }
                    if let Some(expr) = init {
                        stack.push((expr, false));
                    }
                }
                ParsedExpressionKind::Declaration { value, .. } => {
                    stack.push((value, false));
                }
                ParsedExpressionKind::Variable(_) => {}
                ParsedExpressionKind::Literal(lit) => match lit {
                    ParsedLiteral::Struct(t, fields) => {
                        let resolved_type = context.resolved_types.resolve_type(
                            context.module_path,
                            t,
                            context.imports,
                        )?;
                        let struct_decl =
                            context.resolved_types.structs.get(&resolved_type).unwrap();
                        for field in struct_decl.field_order.iter().rev() {
                            stack.push((fields.get(field).unwrap(), false));
                        }
                    }
                    _ => {}
                },
                ParsedExpressionKind::Unary { expr, op } => {
                    match op {
                        UnaryOp::Index(index) => stack.push((index, false)),
                        _ => {}
                    };
                    stack.push((expr, false));
                }
                ParsedExpressionKind::Binary { left, right, .. } => {
                    stack.push((right, false));
                    stack.push((left, false));
                }
                ParsedExpressionKind::FunctionCall { args, .. } => {
                    for arg in args.iter().rev() {
                        stack.push((arg, false));
                    }
                }
                ParsedExpressionKind::MemberFunctionCall { object, args, .. } => {
                    for arg in args.iter().rev() {
                        stack.push((arg, false));
                    }
                    stack.push((object, false));
                }
            }
        } else {
            let merged = match &stack_expr.value {
                ParsedExpressionKind::Block {
                    expressions,
                    returns_value,
                } => {
                    let new_len = output.len() - expressions.len();
                    let merged_expressions = output.split_off(new_len);
                    MergedExpressionKind::Block {
                        expressions: merged_expressions,
                        returns_value: *returns_value,
                    }
                }
                ParsedExpressionKind::Return(maybe_expr) => {
                    let merged_expression =
                        maybe_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    MergedExpressionKind::Return(merged_expression)
                }
                ParsedExpressionKind::Continue => MergedExpressionKind::Continue,
                ParsedExpressionKind::Break(maybe_expr) => {
                    let merged_expression =
                        maybe_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    MergedExpressionKind::Break(merged_expression)
                }
                ParsedExpressionKind::If { else_expr, .. } => {
                    let merged_else_expr =
                        else_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let merged_then_block = Box::new(output.pop().unwrap());
                    let merged_condition = Box::new(output.pop().unwrap());
                    MergedExpressionKind::If {
                        condition: merged_condition,
                        then_block: merged_then_block,
                        else_expr: merged_else_expr,
                    }
                }
                ParsedExpressionKind::Loop {
                    init,
                    condition,
                    step,
                    loop_body: _,
                    else_expr,
                } => {
                    let merged_else_expr =
                        else_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let merged_loop_body = Box::new(output.pop().unwrap());
                    let merged_step = step.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let merged_condition =
                        condition.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let merged_init = init.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    MergedExpressionKind::Loop {
                        init: merged_init,
                        condition: merged_condition,
                        step: merged_step,
                        loop_body: merged_loop_body,
                        else_expr: merged_else_expr,
                    }
                }
                ParsedExpressionKind::Declaration {
                    var_type, var_name, ..
                } => {
                    let merged_value = Box::new(output.pop().unwrap());
                    let resolved_type = if let Some(var_type) = var_type {
                        Some(context.resolved_types.resolve_type(
                            context.module_path,
                            var_type,
                            context.imports,
                        )?)
                    } else {
                        None
                    };
                    MergedExpressionKind::Declaration {
                        var_type: resolved_type,
                        var_name: var_name.clone(),
                        value: merged_value,
                    }
                }
                ParsedExpressionKind::Variable(name) => {
                    MergedExpressionKind::Variable(name.clone())
                }
                ParsedExpressionKind::Literal(lit) => {
                    let lit = match lit {
                        ParsedLiteral::Unit => MergedLiteral::Unit,
                        ParsedLiteral::Bool(b) => MergedLiteral::Bool(*b),
                        ParsedLiteral::Char(c) => MergedLiteral::Char(*c),
                        ParsedLiteral::String(s) => MergedLiteral::String(s.clone()),
                        ParsedLiteral::Integer(i) => MergedLiteral::Integer(*i),
                        ParsedLiteral::Struct(t, _) => {
                            let resolved_type = context.resolved_types.resolve_type(
                                context.module_path,
                                t,
                                context.imports,
                            )?;
                            let struct_decl =
                                context.resolved_types.structs.get(&resolved_type).unwrap();
                            let mut merged_fields = HashMap::new();
                            for field in struct_decl.field_order.iter().rev() {
                                let merged_field = output.pop().unwrap();
                                merged_fields.insert(field.clone(), merged_field);
                            }
                            MergedLiteral::Struct(resolved_type, merged_fields)
                        }
                    };
                    MergedExpressionKind::Literal(lit)
                }
                ParsedExpressionKind::Unary { op, .. } => {
                    let op = match op {
                        UnaryOp::Math(math_op) => MergedUnaryOp::Math(math_op.clone()),
                        UnaryOp::LogicalNot => MergedUnaryOp::LogicalNot,
                        UnaryOp::Borrow => MergedUnaryOp::Borrow,
                        UnaryOp::Dereference => MergedUnaryOp::Dereference,
                        UnaryOp::Increment { is_prefix } => MergedUnaryOp::Increment {
                            is_prefix: *is_prefix,
                        },
                        UnaryOp::Decrement { is_prefix } => MergedUnaryOp::Decrement {
                            is_prefix: *is_prefix,
                        },
                        UnaryOp::Cast(target_type) => {
                            let resolved_type = context.resolved_types.resolve_type(
                                context.module_path,
                                target_type,
                                context.imports,
                            )?;
                            MergedUnaryOp::Cast(resolved_type)
                        }
                        UnaryOp::Member(name) => MergedUnaryOp::Member(name.clone()),
                        UnaryOp::Index(_) => {
                            let merged_index = output.pop().unwrap();
                            MergedUnaryOp::Index(Box::new(merged_index))
                        }
                    };
                    let merged_expr = output.pop().unwrap();
                    MergedExpressionKind::Unary {
                        expr: Box::new(merged_expr),
                        op,
                    }
                }
                ParsedExpressionKind::Binary { op, .. } => {
                    let merged_right = Box::new(output.pop().unwrap());
                    let merged_left = Box::new(output.pop().unwrap());
                    MergedExpressionKind::Binary {
                        left: merged_left,
                        right: merged_right,
                        op: op.clone(),
                    }
                }
                ParsedExpressionKind::FunctionCall { function_id, args } => {
                    let new_len = output.len() - args.len();
                    let merged_args = output.split_off(new_len);
                    let function_header = context
                        .resolved_functions
                        .resolve_function(context.module_path, function_id)
                        .ok_or_else(|| {
                            anyhow::anyhow!("Function '{}' not found at {}", function_id, location)
                        })?;
                    MergedExpressionKind::FunctionCall {
                        function_id: function_header.id.clone(),
                        args: merged_args,
                    }
                }
                ParsedExpressionKind::MemberFunctionCall {
                    object: _,
                    function_name,
                    args,
                } => {
                    let new_len = output.len() - args.len();
                    let merged_args = output.split_off(new_len);
                    let merged_object = output.pop().unwrap();
                    let mut args = vec![merged_object];
                    args.extend(merged_args);

                    MergedExpressionKind::MemberFunctionCall {
                        function_name: function_name.clone(),
                        args,
                    }
                }
            };
            output.push(MergedExpression {
                value: merged,
                location,
            });
        }
    }
    Ok(output.pop().unwrap())
}
