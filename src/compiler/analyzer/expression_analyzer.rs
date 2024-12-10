use crate::compiler::analyzer::analyzed_expression::{
    AssignableExpression, AssignableExpressionKind,
};
use crate::compiler::analyzer::iterative_expression_analyzer::analyze_expression;
use crate::compiler::analyzer::program_analyzer::AnalyzerContext;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::lexer::location::Location;
use crate::compiler::merger::merged_expression::{
    MergedExpression, MergedExpressionKind, MergedUnaryOp, TypeId,
};
use anyhow::Context;

pub fn analyze_assignable_expression(
    context: &mut AnalyzerContext,
    expression: &MergedExpression,
) -> AnalyzerResult<AssignableExpression> {
    match &expression.value {
        MergedExpressionKind::Block { .. } => Err(anyhow::anyhow!(
            "Block expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Return(_) => Err(anyhow::anyhow!(
            "Return expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Continue => Err(anyhow::anyhow!(
            "Continue expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Break(_) => Err(anyhow::anyhow!(
            "Break expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::If { .. } => Err(anyhow::anyhow!(
            "If expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Loop { .. } => Err(anyhow::anyhow!(
            "Loop expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Declaration { .. } => Err(anyhow::anyhow!(
            "Declaration expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::FunctionCall { .. } => Err(anyhow::anyhow!(
            "Function call expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::MemberFunctionCall { .. } => Err(anyhow::anyhow!(
            "Member function call expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Literal(_) => Err(anyhow::anyhow!(
            "Literal expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Binary { .. } => Err(anyhow::anyhow!(
            "Binary expression cannot be assigned to at {}.",
            expression.location
        )),
        MergedExpressionKind::Variable(name) => {
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
        MergedExpressionKind::Unary {
            op: MergedUnaryOp::Dereference,
            expr,
        } => {
            let analyzed_expr = analyze_expression(context, expr).with_context(|| {
                format!(
                    "Failed to analyze assignable dereference expression at {}.",
                    expression.location
                )
            })?;
            match analyzed_expr.ty.clone() {
                TypeId::Pointer(inner) => Ok(AssignableExpression {
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
        MergedExpressionKind::Unary {
            op: MergedUnaryOp::Member(member),
            expr,
        } => try_as_assignable_field_access(context, member.clone(), expr, &expression.location),
        MergedExpressionKind::Unary {
            op: MergedUnaryOp::Index(index),
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
                TypeId::Integer(_) => {}
                _ => Err(anyhow::anyhow!(
                    "Index expression has non-integer type '{}' at {}.",
                    analyzed_index.ty,
                    index.location
                ))?,
            }
            match analyzed_expr.ty.clone() {
                TypeId::Pointer(inner) => Ok(AssignableExpression {
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
        MergedExpressionKind::Unary { .. } => Err(anyhow::anyhow!(
            "Unary expression cannot be assigned to at {}.",
            expression.location
        )),
    }
}

fn try_as_assignable_field_access(
    context: &mut AnalyzerContext,
    member: String,
    inner: &MergedExpression,
    location: &Location,
) -> AnalyzerResult<AssignableExpression> {
    let maybe_assignable_expr = analyze_assignable_expression(context, inner);
    if let Ok(analyzed_expr) = maybe_assignable_expr {
        match &analyzed_expr.ty {
            TypeId::StructType(str_name) => {
                let struct_type = context.structs.get(&analyzed_expr.ty).ok_or_else(|| {
                    anyhow::anyhow!("Struct type '{}' not found at {}.", str_name, location)
                })?;
                let field_type = struct_type.field_types.get(&member).ok_or_else(|| {
                    anyhow::anyhow!(
                        "Struct type '{}' does not have field '{}' at {}.",
                        analyzed_expr.ty,
                        member,
                        location
                    )
                })?;
                return Ok(AssignableExpression {
                    kind: AssignableExpressionKind::FieldAccess(
                        Box::new(analyzed_expr),
                        member.clone(),
                    ),
                    ty: field_type.clone(),
                });
            }
            TypeId::Pointer(_) => {}
            _ => {
                return Err(anyhow::anyhow!(
                    "Expected struct type, found '{}' at {}.",
                    analyzed_expr.ty,
                    location
                ))?
            }
        }
    };

    let analyzed_expr = analyze_expression(context, inner).with_context(|| {
        format!(
            "Failed to analyze assignable member expression at {}.",
            location
        )
    })?;
    match &analyzed_expr.ty {
        TypeId::Pointer(inner) => {
            if let TypeId::StructType(str_name) = inner.as_ref() {
                let struct_type = context.structs.get(inner).ok_or_else(|| {
                    anyhow::anyhow!("Struct type '{}' not found at {}.", str_name, location)
                })?;
                let field_type = struct_type.field_types.get(&member).ok_or_else(|| {
                    anyhow::anyhow!(
                        "Struct type '{}' does not have field '{}' at {}.",
                        inner,
                        member,
                        location
                    )
                })?;
                Ok(AssignableExpression {
                    kind: AssignableExpressionKind::PointerFieldAccess(
                        Box::new(analyzed_expr),
                        member.clone(),
                    ),
                    ty: field_type.clone(),
                })
            } else {
                Err(anyhow::anyhow!(
                    "Expected struct type, found '{}' at {}.",
                    analyzed_expr.ty,
                    location
                ))?
            }
        }
        _ => Err(anyhow::anyhow!(
            "Expected struct type, found '{}' at {}.",
            analyzed_expr.ty,
            location
        )),
    }
}

pub fn can_cast_to(original_type: &TypeId, target_type: &TypeId) -> bool {
    if *original_type == *target_type {
        return true;
    }
    match (original_type, target_type) {
        (TypeId::Integer(_), TypeId::Integer(_)) => true,
        (TypeId::Char, TypeId::Integer(_)) => true,
        (TypeId::Integer(_), TypeId::Char) => true,
        (TypeId::Bool, TypeId::Integer(_)) => true,
        (TypeId::Integer(_), TypeId::Bool) => true,
        (TypeId::Pointer(_), TypeId::Pointer(_)) => true,
        _ => false,
    }
}
