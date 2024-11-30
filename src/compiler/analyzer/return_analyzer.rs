use crate::compiler::analyzer::analyzed_expression::{AnalyzedBinaryOp, AnalyzedExpression, AnalyzedExpressionKind, AnalyzedLiteral, AssignableExpression, AssignableExpressionKind, BinaryAssignOp};

pub fn always_calls_return(expression: &AnalyzedExpression) -> bool {
    match &expression.kind {
        AnalyzedExpressionKind::Block { expressions, returns_value: _ } => {
            expressions.iter().any(always_calls_return)
        }
        AnalyzedExpressionKind::Return(_) => true,
        AnalyzedExpressionKind::Continue => false,
        AnalyzedExpressionKind::Break(expr) => expr.as_ref().is_some_and(|expr| always_calls_return(expr)),
        AnalyzedExpressionKind::If { condition, else_expr, then_block } => {
            always_calls_return(condition) || (else_expr.as_ref().is_some_and(|expr| always_calls_return(expr)) && always_calls_return(then_block))
        }
        AnalyzedExpressionKind::While { condition, else_expr, loop_body } => {
            always_calls_return(condition) || (else_expr.as_ref().is_some_and(|expr| always_calls_return(expr)) && always_calls_return(loop_body))
        }
        AnalyzedExpressionKind::Declaration { var_name: _, value } => always_calls_return(value),
        AnalyzedExpressionKind::ValueOfAssignable(expr) => always_calls_return_assignable(expr),
        AnalyzedExpressionKind::Literal(lit) => match lit {
            AnalyzedLiteral::Struct(fields) => fields.iter().any(|(_, value)| always_calls_return(value)),
            AnalyzedLiteral::Array(values) => values.iter().any(always_calls_return),
            _ => false,
        }
        AnalyzedExpressionKind::Unary { op: _, expr } => always_calls_return(expr),
        AnalyzedExpressionKind::Binary { op, left, right } => match op {
            AnalyzedBinaryOp::Logical(_) => always_calls_return(left),
            _ => always_calls_return(left) || always_calls_return(right),
        }
        AnalyzedExpressionKind::Assign { op, lhs, rhs } => match op {
            BinaryAssignOp::LogicAssign(_) => always_calls_return_assignable(lhs),
            _ => always_calls_return_assignable(lhs) || always_calls_return(rhs),
        }
        AnalyzedExpressionKind::Borrow { expr } => always_calls_return_assignable(expr),
        AnalyzedExpressionKind::FunctionCall { function_name: _, args } => {
            args.iter().any(always_calls_return)
        }
        AnalyzedExpressionKind::FieldAccess { expr, field_name: _ } => always_calls_return(expr),
        AnalyzedExpressionKind::ArrayIndex { array, index } => always_calls_return(array) || always_calls_return(index),
        AnalyzedExpressionKind::Increment(inner, _) => always_calls_return_assignable(inner),
        AnalyzedExpressionKind::Decrement(inner, _) => always_calls_return_assignable(inner),
    }
}

fn always_calls_return_assignable(expression: &AssignableExpression) -> bool {
    match &expression.kind {
        AssignableExpressionKind::LocalVariable(_) => false,
        AssignableExpressionKind::Dereference(expr) => always_calls_return(expr),
        AssignableExpressionKind::FieldAccess(expr, _) => always_calls_return_assignable(expr),
        AssignableExpressionKind::ArrayIndex(arr_expr, index_expr) => always_calls_return(arr_expr) || always_calls_return(index_expr),
    }
}