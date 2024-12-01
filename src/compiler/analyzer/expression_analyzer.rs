use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedBinaryOp, AnalyzedExpression, AnalyzedExpressionKind, AnalyzedLiteral, AnalyzedUnaryOp,
    AssignableExpression, AssignableExpressionKind, BinaryAssignOp,
};
use crate::compiler::analyzer::program_analyzer::{AnalyzerContext, LocalVariable, LoopData};
use crate::compiler::analyzer::type_resolver::AnalyzedType;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::parser::parsed_expression::{
    BinaryComparisonOp, BinaryOp, ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryOp,
};
use anyhow::Context;
use std::collections::HashSet;

pub fn analyze_expression(
    context: &mut AnalyzerContext,
    expression: &ParsedExpression,
) -> AnalyzerResult<AnalyzedExpression> {
    match &expression.value {
        ParsedExpressionKind::Block {
            expressions,
            returns_value,
        } => {
            let old_local_variables = context.local_variables.clone();
            context
                .local_variables
                .values_mut()
                .for_each(|v| v.is_current_scope = false);

            let mut analyzed_expressions = Vec::new();
            for expr in expressions {
                let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze type of expr in block at {}.",
                        expr.location
                    )
                })?;
                analyzed_expressions.push(analyzed_expr);
            }
            let return_ty = if *returns_value {
                analyzed_expressions
                    .last()
                    .map_or(AnalyzedType::Unit, |e| e.ty.clone())
            } else {
                AnalyzedType::Unit
            };

            context.local_variables = old_local_variables;
            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Block {
                    returns_value: *returns_value,
                    expressions: analyzed_expressions,
                },
                ty: return_ty,
            })
        }
        ParsedExpressionKind::Return(expr) => {
            let analyzed_expr = expr
                .as_ref()
                .map(|e| analyze_expression(context, e))
                .transpose()
                .with_context(|| {
                    format!(
                        "Failed to analyze type of return expr at {}.",
                        expression.location
                    )
                })?;
            let return_ty = analyzed_expr
                .as_ref()
                .map_or(AnalyzedType::Unit, |e| e.ty.clone());
            if return_ty != *context.return_type {
                Err(anyhow::anyhow!(
                    "Return type '{}' does not match function return type '{}' at {}.",
                    return_ty,
                    context.return_type,
                    expression.location
                ))?;
            }
            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Return(analyzed_expr.map(Box::new)),
                ty: AnalyzedType::Unit,
            })
        }
        ParsedExpressionKind::Continue => {
            if context.loop_data.is_none() {
                Err(anyhow::anyhow!(
                    "Continue outside of loop at {}.",
                    expression.location
                ))?;
            }
            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Continue,
                ty: AnalyzedType::Unit,
            })
        }
        ParsedExpressionKind::Break(expr) => {
            if context.loop_data.is_none() {
                Err(anyhow::anyhow!(
                    "Break outside of loop at {}.",
                    expression.location
                ))?;
            }
            let analyzed_expr = expr
                .as_ref()
                .map(|e| analyze_expression(context, e))
                .transpose()
                .with_context(|| {
                    format!(
                        "Failed to analyze type of break expr at {}.",
                        expression.location
                    )
                })?;
            let break_return_type = analyzed_expr
                .as_ref()
                .map_or(AnalyzedType::Unit, |e| e.ty.clone());
            let loop_data = context.loop_data.as_ref().unwrap();
            if break_return_type != loop_data.break_return_type {
                Err(anyhow::anyhow!(
                    "Break type '{}' does not match loop break type '{}' at {}.",
                    break_return_type,
                    loop_data.break_return_type,
                    expression.location
                ))?;
            }

            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Break(analyzed_expr.map(Box::new)),
                ty: AnalyzedType::Unit,
            })
        }
        ParsedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            let analyzed_condition = analyze_expression(context, condition).with_context(|| {
                format!(
                    "Failed to analyze type of if condition at {}.",
                    condition.location
                )
            })?;
            if analyzed_condition.ty != AnalyzedType::Bool {
                Err(anyhow::anyhow!(
                    "If condition has non-bool type at {}.",
                    condition.location
                ))?;
            }

            let analyzed_then = analyze_expression(context, then_block).with_context(|| {
                format!(
                    "Failed to analyze type of if then block at {}.",
                    then_block.location
                )
            })?;
            let analyzed_else = else_expr
                .as_ref()
                .map(|e| analyze_expression(context, e))
                .transpose()
                .with_context(|| {
                    format!(
                        "Failed to analyze type of if else block at {}.",
                        expression.location
                    )
                })?;
            let return_ty = if let Some(else_expr) = &analyzed_else {
                if analyzed_then.ty != else_expr.ty {
                    Err(anyhow::anyhow!(
                        "If then block has type '{}', but else block has type '{}' at {}.",
                        analyzed_then.ty,
                        else_expr.ty,
                        expression.location
                    ))?;
                }
                analyzed_then.ty.clone()
            } else {
                if analyzed_then.ty != AnalyzedType::Unit {
                    Err(anyhow::anyhow!(
                        "If then block has non-unit type '{}' but else block is missing at {}.",
                        analyzed_then.ty,
                        expression.location
                    ))?;
                }
                AnalyzedType::Unit
            };
            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::If {
                    condition: Box::new(analyzed_condition),
                    then_block: Box::new(analyzed_then),
                    else_expr: analyzed_else.map(Box::new),
                },
                ty: return_ty,
            })
        }
        ParsedExpressionKind::While {
            condition,
            loop_body,
            else_expr,
        } => {
            let analyzed_condition = analyze_expression(context, condition).with_context(|| {
                format!(
                    "Failed to analyze type of while condition at {}.",
                    condition.location
                )
            })?;
            if analyzed_condition.ty != AnalyzedType::Bool {
                Err(anyhow::anyhow!(
                    "While condition has non-bool type at {}.",
                    condition.location
                ))?;
            }

            let analyzed_else = else_expr
                .as_ref()
                .map(|e| analyze_expression(context, e))
                .transpose()
                .with_context(|| {
                    format!(
                        "Failed to analyze type of while else block at {}.",
                        expression.location
                    )
                })?;

            let old_loop_data = context.loop_data.clone();
            let return_ty = analyzed_else
                .as_ref()
                .map_or(AnalyzedType::Unit, |e| e.ty.clone());
            context.loop_data = Some(LoopData {
                break_return_type: return_ty.clone(),
            });

            let analyzed_body = analyze_expression(context, loop_body).with_context(|| {
                format!(
                    "Failed to analyze type of while loop body at {}.",
                    loop_body.location
                )
            })?;
            if analyzed_body.ty != AnalyzedType::Unit {
                Err(anyhow::anyhow!(
                    "While loop body has non-unit type '{}' at {}.",
                    analyzed_body.ty,
                    loop_body.location
                ))?;
            }

            context.loop_data = old_loop_data;

            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::While {
                    condition: Box::new(analyzed_condition),
                    loop_body: Box::new(analyzed_body),
                    else_expr: analyzed_else.map(Box::new),
                },
                ty: return_ty,
            })
        }
        ParsedExpressionKind::Declaration {
            var_type,
            var_name,
            value,
        } => {
            let analyzed_value = analyze_expression(context, value).with_context(|| {
                format!(
                    "Failed to analyze type of declaration value at {}.",
                    value.location
                )
            })?;
            let maybe_declared_type = var_type
                .as_ref()
                .map(|x| context.analyzed_types.resolve_type(x))
                .transpose()?;
            if let Some(declared_type) = maybe_declared_type {
                if analyzed_value.ty != declared_type {
                    Err(anyhow::anyhow!(
                        "Declaration expression should be of type '{}', but was '{}' at {}.",
                        declared_type,
                        analyzed_value.ty,
                        value.location
                    ))?;
                }
            }
            if let Some(old_var) = context.local_variables.insert(
                var_name.clone(),
                LocalVariable {
                    ty: analyzed_value.ty.clone(),
                    is_current_scope: true,
                },
            ) {
                if old_var.is_current_scope {
                    Err(anyhow::anyhow!(
                        "Variable '{}' already declared at {}.",
                        var_name,
                        expression.location
                    ))?;
                }
            }
            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Declaration {
                    var_name: var_name.clone(),
                    value: Box::new(analyzed_value),
                },
                ty: AnalyzedType::Unit,
            })
        }
        ParsedExpressionKind::Variable(var_name) => {
            let local_var = context.local_variables.get(var_name).ok_or_else(|| {
                anyhow::anyhow!(
                    "Variable '{}' not declared at {}.",
                    var_name,
                    expression.location
                )
            })?;
            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::ValueOfAssignable(AssignableExpression {
                    kind: AssignableExpressionKind::LocalVariable(var_name.clone()),
                    ty: local_var.ty.clone(),
                }),
                ty: local_var.ty.clone(),
            })
        }
        ParsedExpressionKind::Literal(lit) => match lit {
            ParsedLiteral::Unit => Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Unit),
                ty: AnalyzedType::Unit,
            }),
            ParsedLiteral::Bool(val) => Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Bool(*val)),
                ty: AnalyzedType::Bool,
            }),
            ParsedLiteral::Char(val) => Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Char(*val)),
                ty: AnalyzedType::Char,
            }),
            ParsedLiteral::Integer(val) => {
                let ty = if *val >= -2147483648 && *val <= 2147483647 {
                    AnalyzedType::Integer(4)
                } else {
                    AnalyzedType::Integer(8)
                };
                Ok(AnalyzedExpression {
                    kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Integer(*val)),
                    ty,
                })
            }
            ParsedLiteral::String(val) => {
                let bytes = val.as_bytes();
                let array_values = bytes
                    .iter()
                    .map(|b| AnalyzedExpression {
                        kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Char(*b as i8)),
                        ty: AnalyzedType::Char,
                    })
                    .collect();
                Ok(AnalyzedExpression {
                    kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Array(array_values)),
                    ty: AnalyzedType::Array(Box::new(AnalyzedType::Char)),
                })
            }
            ParsedLiteral::Struct(ty, field_values) => {
                let analyzed_ty = context.analyzed_types.resolve_type(ty)?;
                let struct_type = if let AnalyzedType::Struct(str_name) = &analyzed_ty {
                    context
                        .analyzed_types
                        .struct_types
                        .get(str_name)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Struct type '{}' not found at {}.",
                                str_name,
                                ty.location
                            )
                        })?
                } else {
                    Err(anyhow::anyhow!(
                        "Expected struct type, found '{}' at {}.",
                        analyzed_ty,
                        ty.location
                    ))?
                };

                let mut present_fields = field_values.keys().collect::<HashSet<_>>();
                let mut analyzed_field_values = Vec::new();

                for field_name in &struct_type.field_order {
                    let field_value = field_values.get(field_name).ok_or_else(|| {
                        anyhow::anyhow!(
                            "Struct type '{}' is missing field '{}' at {}.",
                            analyzed_ty,
                            field_name,
                            ty.location
                        )
                    })?;
                    let analyzed_field_value = analyze_expression(context, field_value)
                        .with_context(|| {
                            format!(
                                "Failed to analyze type of struct field value at {}.",
                                field_value.location
                            )
                        })?;
                    let expected_type = struct_type.fields.get(field_name).unwrap();
                    if analyzed_field_value.ty != *expected_type {
                        Err(anyhow::anyhow!(
                            "Struct field '{}' has type '{}', but expected '{}' at {}.",
                            field_name,
                            analyzed_field_value.ty,
                            expected_type,
                            field_value.location
                        ))?;
                    }
                    analyzed_field_values.push((field_name.clone(), analyzed_field_value));
                    present_fields.remove(field_name);
                }

                if !present_fields.is_empty() {
                    Err(anyhow::anyhow!(
                        "Struct literal of type '{}' has extra fields: {:?} at {}.",
                        analyzed_ty,
                        present_fields,
                        ty.location
                    ))?;
                }

                Ok(AnalyzedExpression {
                    kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Struct(
                        analyzed_field_values,
                    )),
                    ty: analyzed_ty,
                })
            }
            ParsedLiteral::Array(ty, values) => {
                let mut analyzed_values = Vec::new();
                let array_type = context.analyzed_types.resolve_type(ty)?;
                let element_type = match &array_type {
                    AnalyzedType::Array(inner) => inner.as_ref().clone(),
                    arr_type => Err(anyhow::anyhow!(
                        "Expected array type, found '{}' at {}.",
                        arr_type,
                        expression.location
                    ))?,
                };
                for value in values {
                    let analyzed_value = analyze_expression(context, value).with_context(|| {
                        format!(
                            "Failed to analyze type of array value at {}.",
                            value.location
                        )
                    })?;
                    if analyzed_value.ty != element_type {
                        Err(anyhow::anyhow!(
                            "Array value has type '{}', but expected '{}' at {}.",
                            analyzed_value.ty,
                            element_type,
                            value.location
                        ))?;
                    }

                    analyzed_values.push(analyzed_value);
                }
                Ok(AnalyzedExpression {
                    kind: AnalyzedExpressionKind::Literal(AnalyzedLiteral::Array(analyzed_values)),
                    ty: array_type,
                })
            }
        },
        ParsedExpressionKind::Unary { expr, op } => match op {
            UnaryOp::Math(math_op) => {
                let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze type of math unary expression at {}.",
                        expression.location
                    )
                })?;
                match analyzed_expr.ty {
                    AnalyzedType::Integer(_) => {}
                    _ => Err(anyhow::anyhow!(
                        "Math unary expression has non-integer type '{}' at {}.",
                        analyzed_expr.ty,
                        expression.location
                    ))?,
                }
                Ok(AnalyzedExpression {
                    ty: analyzed_expr.ty.clone(),
                    kind: AnalyzedExpressionKind::Unary {
                        op: AnalyzedUnaryOp::Math(math_op.clone()),
                        expr: Box::new(analyzed_expr),
                    },
                })
            }
            UnaryOp::LogicalNot => {
                let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze type of logical not expression at {}.",
                        expression.location
                    )
                })?;
                if analyzed_expr.ty != AnalyzedType::Bool {
                    Err(anyhow::anyhow!(
                        "Logical not expression has non-bool type '{}' at {}.",
                        analyzed_expr.ty,
                        expression.location
                    ))?;
                }
                Ok(AnalyzedExpression {
                    kind: AnalyzedExpressionKind::Unary {
                        op: AnalyzedUnaryOp::LogicalNot,
                        expr: Box::new(analyzed_expr),
                    },
                    ty: AnalyzedType::Bool,
                })
            }
            UnaryOp::Borrow => {
                let analyzed_expr =
                    analyze_assignable_expression(context, expr).with_context(|| {
                        format!(
                            "Failed to analyze type of borrow expression at {}.",
                            expression.location
                        )
                    })?;
                Ok(AnalyzedExpression {
                    ty: AnalyzedType::Pointer(Box::new(analyzed_expr.ty.clone())),
                    kind: AnalyzedExpressionKind::Borrow {
                        expr: analyzed_expr,
                    },
                })
            }
            UnaryOp::Dereference => {
                let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze type of dereference expression at {}.",
                        expression.location
                    )
                })?;
                match analyzed_expr.ty.clone() {
                    AnalyzedType::Pointer(inner) => Ok(AnalyzedExpression {
                        kind: AnalyzedExpressionKind::ValueOfAssignable(AssignableExpression {
                            kind: AssignableExpressionKind::Dereference(Box::new(analyzed_expr)),
                            ty: *inner.clone(),
                        }),
                        ty: *inner,
                    }),
                    _ => Err(anyhow::anyhow!(
                        "Dereference expression has non-pointer type '{}' at {}.",
                        analyzed_expr.ty,
                        expression.location
                    ))?,
                }
            }
            UnaryOp::Increment { is_prefix } => {
                let analyzed_expr =
                    analyze_assignable_expression(context, expr).with_context(|| {
                        format!(
                            "Failed to analyze type of increment expression at {}.",
                            expression.location
                        )
                    })?;
                match &analyzed_expr.ty {
                    AnalyzedType::Integer(_) => {}
                    _ => Err(anyhow::anyhow!(
                        "Increment expression has non-integer type '{}' at {}.",
                        analyzed_expr.ty,
                        expression.location
                    ))?,
                }
                Ok(AnalyzedExpression {
                    ty: analyzed_expr.ty.clone(),
                    kind: AnalyzedExpressionKind::Increment(analyzed_expr, *is_prefix),
                })
            }
            UnaryOp::Decrement { is_prefix } => {
                let analyzed_expr =
                    analyze_assignable_expression(context, expr).with_context(|| {
                        format!(
                            "Failed to analyze type of decrement expression at {}.",
                            expression.location
                        )
                    })?;
                match &analyzed_expr.ty {
                    AnalyzedType::Integer(_) => {}
                    _ => Err(anyhow::anyhow!(
                        "Decrement expression has non-integer type '{}' at {}.",
                        analyzed_expr.ty,
                        expression.location
                    ))?,
                }
                Ok(AnalyzedExpression {
                    ty: analyzed_expr.ty.clone(),
                    kind: AnalyzedExpressionKind::Decrement(analyzed_expr, *is_prefix),
                })
            }
            UnaryOp::Cast(target_type) => {
                let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze type of cast expression at {}.",
                        expression.location
                    )
                })?;
                let target_type = context.analyzed_types.resolve_type(target_type)?;
                if analyzed_expr.ty.can_cast_to(&target_type) {
                    Ok(AnalyzedExpression {
                        kind: AnalyzedExpressionKind::Unary {
                            op: AnalyzedUnaryOp::Cast,
                            expr: Box::new(analyzed_expr),
                        },
                        ty: target_type,
                    })
                } else {
                    Err(anyhow::anyhow!(
                        "Cast expression has type '{}', but expected '{}' at {}.",
                        analyzed_expr.ty,
                        target_type,
                        expression.location
                    ))?
                }
            }
            UnaryOp::Member(member) => {
                let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze type of member expression at {}.",
                        expression.location
                    )
                })?;
                let struct_type = match &analyzed_expr.ty {
                    AnalyzedType::Struct(str_name) => context
                        .analyzed_types
                        .struct_types
                        .get(str_name)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Struct type '{}' not found at {}.",
                                str_name,
                                expr.location
                            )
                        })?,
                    _ => Err(anyhow::anyhow!(
                        "Expected struct type, found '{}' at {}.",
                        analyzed_expr.ty,
                        expr.location
                    ))?,
                };
                let field_type = struct_type.fields.get(member).ok_or_else(|| {
                    anyhow::anyhow!(
                        "Struct type '{}' does not have field '{}' at {}.",
                        analyzed_expr.ty,
                        member,
                        expr.location
                    )
                })?;
                Ok(AnalyzedExpression {
                    kind: AnalyzedExpressionKind::FieldAccess {
                        field_name: member.clone(),
                        expr: Box::new(analyzed_expr),
                    },
                    ty: field_type.clone(),
                })
            }
            UnaryOp::Index(index) => {
                let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze type of index expression at {}.",
                        expression.location
                    )
                })?;
                let analyzed_index = analyze_expression(context, index).with_context(|| {
                    format!(
                        "Failed to analyze type of index expression at {}.",
                        expression.location
                    )
                })?;
                match &analyzed_index.ty {
                    AnalyzedType::Integer(_) => {}
                    _ => Err(anyhow::anyhow!(
                        "Index expression has non-integer type '{}' at {}.",
                        analyzed_index.ty,
                        index.location
                    ))?,
                }
                match analyzed_expr.ty.clone() {
                    AnalyzedType::Array(inner) => Ok(AnalyzedExpression {
                        kind: AnalyzedExpressionKind::ArrayIndex {
                            array: Box::new(analyzed_expr),
                            index: Box::new(analyzed_index),
                        },
                        ty: *inner,
                    }),
                    _ => Err(anyhow::anyhow!(
                        "Index expression has non-array type '{}' at {}.",
                        analyzed_expr.ty,
                        expr.location
                    ))?,
                }
            }
        },
        ParsedExpressionKind::Binary { left, op, right } => {
            match op {
                BinaryOp::Math(math_op) => {
                    let analyzed_left = analyze_expression(context, left).with_context(|| {
                        format!(
                            "Failed to analyze type of math binary left expression at {}.",
                            expression.location
                        )
                    })?;
                    let analyzed_right = analyze_expression(context, right).with_context(|| {
                        format!(
                            "Failed to analyze type of math binary right expression at {}.",
                            expression.location
                        )
                    })?;
                    match &analyzed_left.ty {
                        AnalyzedType::Integer(_) => {}
                        _ => Err(anyhow::anyhow!(
                            "Math binary left expression has non-integer type '{}' at {}.",
                            analyzed_left.ty,
                            left.location
                        ))?,
                    }
                    if analyzed_left.ty != analyzed_right.ty {
                        Err(anyhow::anyhow!("Math binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                    }
                    Ok(AnalyzedExpression {
                        ty: analyzed_left.ty.clone(),
                        kind: AnalyzedExpressionKind::Binary {
                            op: AnalyzedBinaryOp::Math(math_op.clone()),
                            left: Box::new(analyzed_left),
                            right: Box::new(analyzed_right),
                        },
                    })
                }
                BinaryOp::Logical(logic_op) => {
                    let analyzed_left = analyze_expression(context, left).with_context(|| {
                        format!(
                            "Failed to analyze type of logic binary left expression at {}.",
                            expression.location
                        )
                    })?;
                    let analyzed_right = analyze_expression(context, right).with_context(|| {
                        format!(
                            "Failed to analyze type of logic binary right expression at {}.",
                            expression.location
                        )
                    })?;
                    if analyzed_left.ty != AnalyzedType::Bool {
                        Err(anyhow::anyhow!(
                            "Logic binary left expression has non-bool type '{}' at {}.",
                            analyzed_left.ty,
                            left.location
                        ))?;
                    }
                    if analyzed_right.ty != AnalyzedType::Bool {
                        Err(anyhow::anyhow!(
                            "Logic binary right expression has non-bool type '{}' at {}.",
                            analyzed_right.ty,
                            right.location
                        ))?;
                    }
                    Ok(AnalyzedExpression {
                        kind: AnalyzedExpressionKind::Binary {
                            op: AnalyzedBinaryOp::Logical(logic_op.clone()),
                            left: Box::new(analyzed_left),
                            right: Box::new(analyzed_right),
                        },
                        ty: AnalyzedType::Bool,
                    })
                }
                BinaryOp::Comparison(comp_op) => {
                    let analyzed_left = analyze_expression(context, left).with_context(|| {
                        format!(
                            "Failed to analyze type of comparison binary left expression at {}.",
                            expression.location
                        )
                    })?;
                    let analyzed_right = analyze_expression(context, right).with_context(|| {
                        format!(
                            "Failed to analyze type of comparison binary right expression at {}.",
                            expression.location
                        )
                    })?;
                    let needs_integers = match comp_op {
                        BinaryComparisonOp::Equals | BinaryComparisonOp::NotEquals => false,
                        _ => true,
                    };
                    match &analyzed_left.ty {
                        AnalyzedType::Integer(_) => {}
                        AnalyzedType::Char | AnalyzedType::Bool if !needs_integers => {}
                        _ => Err(anyhow::anyhow!(
                            "Comparison binary left expression has non-comparable type '{}' at {}.",
                            analyzed_left.ty,
                            left.location
                        ))?,
                    }
                    if analyzed_left.ty != analyzed_right.ty {
                        Err(anyhow::anyhow!("Comparison binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                    }
                    Ok(AnalyzedExpression {
                        kind: AnalyzedExpressionKind::Binary {
                            op: AnalyzedBinaryOp::Comparison(comp_op.clone()),
                            left: Box::new(analyzed_left),
                            right: Box::new(analyzed_right),
                        },
                        ty: AnalyzedType::Bool,
                    })
                }
                BinaryOp::Assign => {
                    let analyzed_left =
                        analyze_assignable_expression(context, left).with_context(|| {
                            format!(
                                "Failed to analyze type of assign binary left expression at {}.",
                                expression.location
                            )
                        })?;
                    let analyzed_right = analyze_expression(context, right).with_context(|| {
                        format!(
                            "Failed to analyze type of assign binary right expression at {}.",
                            expression.location
                        )
                    })?;
                    if analyzed_left.ty != analyzed_right.ty {
                        Err(anyhow::anyhow!("Assign binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                    }
                    Ok(AnalyzedExpression {
                        ty: analyzed_left.ty.clone(),
                        kind: AnalyzedExpressionKind::Assign {
                            op: BinaryAssignOp::Assign,
                            lhs: analyzed_left,
                            rhs: Box::new(analyzed_right),
                        },
                    })
                }
                BinaryOp::MathAssign(math_op) => {
                    let analyzed_left = analyze_assignable_expression(context, left).with_context(|| format!("Failed to analyze type of math assign binary left expression at {}.", expression.location))?;
                    let analyzed_right = analyze_expression(context, right).with_context(|| {
                        format!(
                            "Failed to analyze type of math assign binary right expression at {}.",
                            expression.location
                        )
                    })?;
                    match &analyzed_left.ty {
                        AnalyzedType::Integer(_) => {}
                        _ => Err(anyhow::anyhow!(
                            "Math assign binary left expression has non-integer type '{}' at {}.",
                            analyzed_left.ty,
                            left.location
                        ))?,
                    }
                    if analyzed_left.ty != analyzed_right.ty {
                        Err(anyhow::anyhow!("Math assign binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                    }
                    Ok(AnalyzedExpression {
                        ty: analyzed_left.ty.clone(),
                        kind: AnalyzedExpressionKind::Assign {
                            op: BinaryAssignOp::MathAssign(math_op.clone()),
                            lhs: analyzed_left,
                            rhs: Box::new(analyzed_right),
                        },
                    })
                }
                BinaryOp::LogicAssign(logic_op) => {
                    let analyzed_left = analyze_assignable_expression(context, left).with_context(|| format!("Failed to analyze type of logic assign binary left expression at {}.", expression.location))?;
                    let analyzed_right = analyze_expression(context, right).with_context(|| {
                        format!(
                            "Failed to analyze type of logic assign binary right expression at {}.",
                            expression.location
                        )
                    })?;
                    if analyzed_left.ty != AnalyzedType::Bool {
                        Err(anyhow::anyhow!(
                            "Logic assign binary left expression has non-bool type '{}' at {}.",
                            analyzed_left.ty,
                            left.location
                        ))?;
                    }
                    if analyzed_right.ty != AnalyzedType::Bool {
                        Err(anyhow::anyhow!(
                            "Logic assign binary right expression has non-bool type '{}' at {}.",
                            analyzed_right.ty,
                            right.location
                        ))?;
                    }
                    Ok(AnalyzedExpression {
                        kind: AnalyzedExpressionKind::Assign {
                            op: BinaryAssignOp::LogicAssign(logic_op.clone()),
                            lhs: analyzed_left,
                            rhs: Box::new(analyzed_right),
                        },
                        ty: AnalyzedType::Bool,
                    })
                }
            }
        }
        ParsedExpressionKind::FunctionCall {
            function_name,
            args,
        } => {
            let function_header = context.function_headers.get(function_name).ok_or_else(|| {
                anyhow::anyhow!(
                    "Function '{}' not found at {}.",
                    function_name,
                    expression.location
                )
            })?;

            if args.len() != function_header.parameters.len() {
                Err(anyhow::anyhow!(
                    "Function '{}' expects {} arguments, but got {} at {}.",
                    function_name,
                    function_header.parameters.len(),
                    args.len(),
                    expression.location
                ))?;
            }

            let mut analyzed_args = Vec::new();
            for (arg, arg_type) in args.iter().zip(function_header.parameters.iter()) {
                let analyzed_arg = analyze_expression(context, arg).with_context(|| {
                    format!(
                        "Failed to analyze type of function call argument at {}.",
                        arg.location
                    )
                })?;
                if analyzed_arg.ty != *arg_type {
                    Err(anyhow::anyhow!(
                        "Function call argument has type '{}', but expected '{}' at {}.",
                        analyzed_arg.ty,
                        arg_type,
                        arg.location
                    ))?;
                }
                analyzed_args.push(analyzed_arg);
            }
            Ok(AnalyzedExpression {
                kind: AnalyzedExpressionKind::FunctionCall {
                    function_name: function_name.clone(),
                    args: analyzed_args,
                },
                ty: function_header.return_type.clone(),
            })
        }
    }
}

pub fn analyze_assignable_expression(
    context: &mut AnalyzerContext,
    expression: &ParsedExpression,
) -> AnalyzerResult<AssignableExpression> {
    match &expression.value {
        ParsedExpressionKind::Block { .. } => Err(anyhow::anyhow!(
            "Block expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::Return(_) => Err(anyhow::anyhow!(
            "Return expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::Continue => Err(anyhow::anyhow!(
            "Continue expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::Break(_) => Err(anyhow::anyhow!(
            "Break expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::If { .. } => Err(anyhow::anyhow!(
            "If expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::While { .. } => Err(anyhow::anyhow!(
            "While expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::Declaration { .. } => Err(anyhow::anyhow!(
            "Declaration expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::FunctionCall { .. } => Err(anyhow::anyhow!(
            "Function call expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::Literal(_) => Err(anyhow::anyhow!(
            "Literal expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::Binary { .. } => Err(anyhow::anyhow!(
            "Binary expression cannot be assigned to at {}.",
            expression.location
        )),
        ParsedExpressionKind::Variable(name) => {
            let var_type = context.local_variables.get(name).ok_or_else(|| {
                anyhow::anyhow!(
                    "Variable '{}' not declared at {}.",
                    name,
                    expression.location
                )
            })?;
            Ok(AssignableExpression {
                kind: AssignableExpressionKind::LocalVariable(name.clone()),
                ty: var_type.ty.clone(),
            })
        }
        ParsedExpressionKind::Unary {
            op: UnaryOp::Dereference,
            expr,
        } => {
            let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                format!(
                    "Failed to analyze assignable dereference expression at {}.",
                    expression.location
                )
            })?;
            match analyzed_expr.ty.clone() {
                AnalyzedType::Pointer(inner) => Ok(AssignableExpression {
                    kind: AssignableExpressionKind::Dereference(Box::new(analyzed_expr)),
                    ty: *inner,
                }),
                _ => Err(anyhow::anyhow!(
                    "Dereference expression has non-pointer type '{}' at {}.",
                    analyzed_expr.ty,
                    expr.location
                )),
            }
        }
        ParsedExpressionKind::Unary {
            op: UnaryOp::Member(member),
            expr,
        } => {
            let analyzed_expr =
                analyze_assignable_expression(context, expr).with_context(|| {
                    format!(
                        "Failed to analyze assignable member expression at {}.",
                        expression.location
                    )
                })?;
            let field_ty = match &analyzed_expr.ty {
                AnalyzedType::Struct(str_name) => {
                    let struct_type = context
                        .analyzed_types
                        .struct_types
                        .get(str_name)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Struct type '{}' not found at {}.",
                                str_name,
                                expr.location
                            )
                        })?;
                    struct_type.fields.get(member).ok_or_else(|| {
                        anyhow::anyhow!(
                            "Struct type '{}' does not have field '{}' at {}.",
                            analyzed_expr.ty,
                            member,
                            expr.location
                        )
                    })?
                }
                _ => Err(anyhow::anyhow!(
                    "Expected struct type, found '{}' at {}.",
                    analyzed_expr.ty,
                    expr.location
                ))?,
            };
            Ok(AssignableExpression {
                ty: field_ty.clone(),
                kind: AssignableExpressionKind::FieldAccess(
                    Box::new(analyzed_expr),
                    member.clone(),
                ),
            })
        }
        ParsedExpressionKind::Unary {
            op: UnaryOp::Index(index),
            expr,
        } => {
            let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                format!(
                    "Failed to analyze array expression at {}.",
                    expression.location
                )
            })?;
            let analyzed_index = analyze_expression(context, index).with_context(|| {
                format!(
                    "Failed to analyze array index expression at {}.",
                    expression.location
                )
            })?;
            match &analyzed_index.ty {
                AnalyzedType::Integer(_) => {}
                _ => Err(anyhow::anyhow!(
                    "Index expression has non-integer type '{}' at {}.",
                    analyzed_index.ty,
                    index.location
                ))?,
            }
            match analyzed_expr.ty.clone() {
                AnalyzedType::Array(inner) => Ok(AssignableExpression {
                    ty: *inner,
                    kind: AssignableExpressionKind::ArrayIndex(
                        Box::new(analyzed_expr),
                        Box::new(analyzed_index),
                    ),
                }),
                _ => Err(anyhow::anyhow!(
                    "Index expression has non-array type '{}' at {}.",
                    analyzed_expr.ty,
                    expr.location
                )),
            }
        }
        ParsedExpressionKind::Unary { .. } => Err(anyhow::anyhow!(
            "Unary expression cannot be assigned to at {}.",
            expression.location
        )),
    }
}
