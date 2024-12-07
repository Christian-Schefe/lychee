use crate::compiler::merger::merged_expression::{
    MergedExpression, MergedExpressionKind, MergedLiteral, MergedUnaryOp, ResolvedFunctions,
    ResolvedTypes,
};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{
    ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryOp,
};
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

pub fn merge_expression(
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    module_path: &ModuleIdentifier,
    parsed_expression: &ParsedExpression,
) -> MergerResult<MergedExpression> {
    let mut stack = vec![(parsed_expression, false)];
    let mut output = vec![];
    while let Some((stack_expr, was_visited)) = stack.pop() {
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
                ParsedExpressionKind::While {
                    condition,
                    loop_body,
                    else_expr,
                } => {
                    if let Some(expr) = else_expr {
                        stack.push((expr, false));
                    }
                    stack.push((loop_body, false));
                    stack.push((condition, false));
                }
                ParsedExpressionKind::For {
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
                    stack.push((step, false));
                    stack.push((condition, false));
                    stack.push((init, false));
                }
                ParsedExpressionKind::Declaration { value, .. } => {
                    stack.push((value, false));
                }
                ParsedExpressionKind::Variable(_) => {}
                ParsedExpressionKind::Literal(lit) => match lit {
                    ParsedLiteral::Struct(t, fields) => {
                        let resolved_type = resolved_types.resolve_type(module_path, t)?;
                        let struct_decl = resolved_types.structs.get(&resolved_type).unwrap();
                        for field in struct_decl.field_order.iter().rev() {
                            stack.push((fields.get(field).unwrap(), false));
                        }
                        continue;
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
            }
        } else {
            let merged = match &stack_expr.value {
                ParsedExpressionKind::Block {
                    expressions,
                    returns_value,
                } => {
                    let new_len = output.len() - expressions.len();
                    let merged_expressions = output.split_off(new_len);
                    MergedExpression {
                        value: MergedExpressionKind::Block {
                            expressions: merged_expressions,
                            returns_value: *returns_value,
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::Return(maybe_expr) => {
                    let merged_expression =
                        maybe_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    MergedExpression {
                        value: MergedExpressionKind::Return(merged_expression),
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::Continue => MergedExpression {
                    value: MergedExpressionKind::Continue,
                    location: parsed_expression.location.clone(),
                },
                ParsedExpressionKind::Break(maybe_expr) => {
                    let merged_expression =
                        maybe_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    MergedExpression {
                        value: MergedExpressionKind::Break(merged_expression),
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::If { else_expr, .. } => {
                    let merged_else_expr =
                        else_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let merged_then_block = Box::new(output.pop().unwrap());
                    let merged_condition = Box::new(output.pop().unwrap());
                    MergedExpression {
                        value: MergedExpressionKind::If {
                            condition: merged_condition,
                            then_block: merged_then_block,
                            else_expr: merged_else_expr,
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::While { else_expr, .. } => {
                    let merged_else_expr =
                        else_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let merged_loop_body = Box::new(output.pop().unwrap());
                    let merged_condition = Box::new(output.pop().unwrap());
                    MergedExpression {
                        value: MergedExpressionKind::While {
                            condition: merged_condition,
                            loop_body: merged_loop_body,
                            else_expr: merged_else_expr,
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::For { else_expr, .. } => {
                    let merged_else_expr =
                        else_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let merged_loop_body = Box::new(output.pop().unwrap());
                    let merged_step = Box::new(output.pop().unwrap());
                    let merged_condition = Box::new(output.pop().unwrap());
                    let merged_init = Box::new(output.pop().unwrap());
                    MergedExpression {
                        value: MergedExpressionKind::For {
                            init: merged_init,
                            condition: merged_condition,
                            step: merged_step,
                            loop_body: merged_loop_body,
                            else_expr: merged_else_expr,
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::Declaration {
                    var_type, var_name, ..
                } => {
                    let merged_value = Box::new(output.pop().unwrap());
                    let resolved_type = if let Some(var_type) = var_type {
                        Some(resolved_types.resolve_type(module_path, var_type)?)
                    } else {
                        None
                    };
                    MergedExpression {
                        value: MergedExpressionKind::Declaration {
                            var_type: resolved_type,
                            var_name: var_name.clone(),
                            value: merged_value,
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::Variable(name) => MergedExpression {
                    value: MergedExpressionKind::Variable(name.clone()),
                    location: parsed_expression.location.clone(),
                },
                ParsedExpressionKind::Literal(lit) => {
                    let lit = match lit {
                        ParsedLiteral::Unit => MergedLiteral::Unit,
                        ParsedLiteral::Bool(b) => MergedLiteral::Bool(*b),
                        ParsedLiteral::Char(c) => MergedLiteral::Char(*c),
                        ParsedLiteral::String(s) => MergedLiteral::String(s.clone()),
                        ParsedLiteral::Integer(i) => MergedLiteral::Integer(*i),
                        ParsedLiteral::Struct(t, _) => {
                            let resolved_type = resolved_types.resolve_type(module_path, t)?;
                            let struct_decl = resolved_types.structs.get(&resolved_type).unwrap();
                            let mut merged_fields = HashMap::new();
                            for field in struct_decl.field_order.iter().rev() {
                                let merged_field = output.pop().unwrap();
                                merged_fields.insert(field.clone(), merged_field);
                            }
                            MergedLiteral::Struct(resolved_type, merged_fields)
                        }
                    };
                    MergedExpression {
                        value: MergedExpressionKind::Literal(lit),
                        location: parsed_expression.location.clone(),
                    }
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
                            let resolved_type =
                                resolved_types.resolve_type(module_path, target_type)?;
                            MergedUnaryOp::Cast(resolved_type)
                        }
                        UnaryOp::Member(name) => MergedUnaryOp::Member(name.clone()),
                        UnaryOp::Index(_) => {
                            let merged_index = output.pop().unwrap();
                            MergedUnaryOp::Index(Box::new(merged_index))
                        }
                    };
                    let merged_expr = output.pop().unwrap();
                    MergedExpression {
                        value: MergedExpressionKind::Unary {
                            expr: Box::new(merged_expr),
                            op,
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::Binary { op, .. } => {
                    let merged_right = Box::new(output.pop().unwrap());
                    let merged_left = Box::new(output.pop().unwrap());
                    MergedExpression {
                        value: MergedExpressionKind::Binary {
                            left: merged_left,
                            right: merged_right,
                            op: op.clone(),
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
                ParsedExpressionKind::FunctionCall { function_id, args } => {
                    let new_len = output.len() - args.len();
                    let merged_args = output.split_off(new_len);
                    let function_header = resolved_functions
                        .resolve_function(module_path, function_id)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Function '{}' not found at {}",
                                function_id,
                                parsed_expression.location
                            )
                        })?;
                    MergedExpression {
                        value: MergedExpressionKind::FunctionCall {
                            function_id: function_header.id.clone(),
                            args: merged_args,
                        },
                        location: parsed_expression.location.clone(),
                    }
                }
            };
            output.push(merged);
        }
    }
    Ok(output.pop().unwrap())
}
