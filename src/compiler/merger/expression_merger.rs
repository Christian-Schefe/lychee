use crate::compiler::merger::merged_expression::{
    MergedExpression, MergedExpressionKind, MergedLiteral, MergedUnaryOp, ResolvedFunctions,
    ResolvedTypes,
};
use crate::compiler::merger::MergerResult;
use crate::compiler::parser::parsed_expression::{
    ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryOp,
};
use crate::compiler::parser::ModulePath;
use std::collections::HashMap;

pub fn merge_expression(
    resolved_types: &ResolvedTypes,
    resolved_functions: &ResolvedFunctions,
    module_path: &ModulePath,
    parsed_expression: &ParsedExpression,
) -> MergerResult<MergedExpression> {
    match &parsed_expression.value {
        ParsedExpressionKind::Block {
            expressions,
            returns_value,
        } => {
            let mut merged_expressions = Vec::new();
            for expression in expressions {
                let merged_expression =
                    merge_expression(resolved_types, resolved_functions, module_path, expression)?;
                merged_expressions.push(merged_expression);
            }
            Ok(MergedExpression {
                value: MergedExpressionKind::Block {
                    expressions: merged_expressions,
                    returns_value: *returns_value,
                },
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::Return(maybe_expr) => {
            let value = if let Some(expr) = maybe_expr {
                Some(Box::new(merge_expression(
                    resolved_types,
                    resolved_functions,
                    module_path,
                    expr,
                )?))
            } else {
                None
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::Return(value),
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::Continue => Ok(MergedExpression {
            value: MergedExpressionKind::Continue,
            location: parsed_expression.location.clone(),
        }),
        ParsedExpressionKind::Break(maybe_expr) => {
            let value = if let Some(expr) = maybe_expr {
                Some(Box::new(merge_expression(
                    resolved_types,
                    resolved_functions,
                    module_path,
                    expr,
                )?))
            } else {
                None
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::Break(value),
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            let merged_condition =
                merge_expression(resolved_types, resolved_functions, module_path, condition)?;
            let merged_then_block =
                merge_expression(resolved_types, resolved_functions, module_path, then_block)?;
            let merged_else_expr = if let Some(expr) = else_expr {
                Some(Box::new(merge_expression(
                    resolved_types,
                    resolved_functions,
                    module_path,
                    expr,
                )?))
            } else {
                None
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::If {
                    condition: Box::new(merged_condition),
                    then_block: Box::new(merged_then_block),
                    else_expr: merged_else_expr,
                },
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::While {
            condition,
            loop_body,
            else_expr,
        } => {
            let merged_condition =
                merge_expression(resolved_types, resolved_functions, module_path, condition)?;
            let merged_loop_body =
                merge_expression(resolved_types, resolved_functions, module_path, loop_body)?;
            let merged_else_expr = if let Some(expr) = else_expr {
                Some(Box::new(merge_expression(
                    resolved_types,
                    resolved_functions,
                    module_path,
                    expr,
                )?))
            } else {
                None
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::While {
                    condition: Box::new(merged_condition),
                    loop_body: Box::new(merged_loop_body),
                    else_expr: merged_else_expr,
                },
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::For {
            init,
            condition,
            step,
            loop_body,
            else_expr,
        } => {
            let merged_init =
                merge_expression(resolved_types, resolved_functions, module_path, init)?;
            let merged_condition =
                merge_expression(resolved_types, resolved_functions, module_path, condition)?;
            let merged_step =
                merge_expression(resolved_types, resolved_functions, module_path, step)?;
            let merged_loop_body =
                merge_expression(resolved_types, resolved_functions, module_path, loop_body)?;
            let merged_else_expr = if let Some(expr) = else_expr {
                Some(Box::new(merge_expression(
                    resolved_types,
                    resolved_functions,
                    module_path,
                    expr,
                )?))
            } else {
                None
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::For {
                    init: Box::new(merged_init),
                    condition: Box::new(merged_condition),
                    step: Box::new(merged_step),
                    loop_body: Box::new(merged_loop_body),
                    else_expr: merged_else_expr,
                },
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::Declaration {
            var_type,
            value,
            var_name,
        } => {
            let merged_value =
                merge_expression(resolved_types, resolved_functions, module_path, value)?;
            let resolved_type = if let Some(var_type) = var_type {
                Some(resolved_types.resolve_type(module_path, var_type)?)
            } else {
                None
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::Declaration {
                    var_type: resolved_type,
                    var_name: var_name.clone(),
                    value: Box::new(merged_value),
                },
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::Variable(name) => Ok(MergedExpression {
            value: MergedExpressionKind::Variable(name.clone()),
            location: parsed_expression.location.clone(),
        }),
        ParsedExpressionKind::Literal(lit) => {
            let lit = match lit {
                ParsedLiteral::Unit => MergedLiteral::Unit,
                ParsedLiteral::Bool(b) => MergedLiteral::Bool(*b),
                ParsedLiteral::Char(c) => MergedLiteral::Char(*c),
                ParsedLiteral::String(s) => MergedLiteral::String(s.clone()),
                ParsedLiteral::Integer(i) => MergedLiteral::Integer(*i),
                ParsedLiteral::Struct(t, fields) => {
                    let resolved_type = resolved_types.resolve_type(module_path, t)?;
                    let mut merged_fields = HashMap::new();
                    for (field_name, field_expr) in fields {
                        let merged_expr = merge_expression(
                            resolved_types,
                            resolved_functions,
                            module_path,
                            field_expr,
                        )?;
                        merged_fields.insert(field_name.clone(), merged_expr);
                    }
                    MergedLiteral::Struct(resolved_type, merged_fields)
                }
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::Literal(lit),
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::Unary { expr, op } => {
            let merged_expr =
                merge_expression(resolved_types, resolved_functions, module_path, expr)?;
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
                    let resolved_type = resolved_types.resolve_type(module_path, target_type)?;
                    MergedUnaryOp::Cast(resolved_type)
                }
                UnaryOp::Member(name) => MergedUnaryOp::Member(name.clone()),
                UnaryOp::Index(index) => {
                    let merged_index =
                        merge_expression(resolved_types, resolved_functions, module_path, index)?;
                    MergedUnaryOp::Index(Box::new(merged_index))
                }
            };
            Ok(MergedExpression {
                value: MergedExpressionKind::Unary {
                    op,
                    expr: Box::new(merged_expr),
                },
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::Binary { left, right, op } => {
            let merged_left =
                merge_expression(resolved_types, resolved_functions, module_path, left)?;
            let merged_right =
                merge_expression(resolved_types, resolved_functions, module_path, right)?;
            Ok(MergedExpression {
                value: MergedExpressionKind::Binary {
                    op: op.clone(),
                    left: Box::new(merged_left),
                    right: Box::new(merged_right),
                },
                location: parsed_expression.location.clone(),
            })
        }
        ParsedExpressionKind::FunctionCall { function_id, args } => {
            let mut merged_args = Vec::new();
            for arg in args {
                let merged_arg =
                    merge_expression(resolved_types, resolved_functions, module_path, arg)?;
                merged_args.push(merged_arg);
            }
            let function_header = resolved_functions
                .resolve_function(module_path, function_id)
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "Function '{}' not found at {}",
                        function_id,
                        parsed_expression.location
                    )
                })?;
            Ok(MergedExpression {
                value: MergedExpressionKind::FunctionCall {
                    function_id: function_header.id.clone(),
                    args: merged_args,
                },
                location: parsed_expression.location.clone(),
            })
        }
    }
}
