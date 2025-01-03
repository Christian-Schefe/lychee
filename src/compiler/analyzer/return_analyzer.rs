use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedBinaryOp, AnalyzedExpression, AnalyzedExpressionKind, AnalyzedFunctionCallType,
    AssignableExpression, AssignableExpressionKind, BinaryAssignOp,
};

pub fn always_calls_return(expression: &AnalyzedExpression) -> bool {
    match &expression.kind {
        AnalyzedExpressionKind::Block {
            expressions,
            returns_value: _,
        } => expressions.iter().any(always_calls_return),
        AnalyzedExpressionKind::Return(_) => true,
        AnalyzedExpressionKind::Continue => false,
        AnalyzedExpressionKind::Break(expr) => {
            expr.as_ref().is_some_and(|expr| always_calls_return(expr))
        }
        AnalyzedExpressionKind::If {
            condition,
            else_expr,
            then_block,
        } => {
            always_calls_return(condition)
                || (else_expr
                    .as_ref()
                    .is_some_and(|expr| always_calls_return(expr))
                    && always_calls_return(then_block))
        }
        AnalyzedExpressionKind::Loop {
            init,
            condition,
            step: _,
            else_expr,
            loop_body,
        } => {
            init.as_ref().is_some_and(|expr| always_calls_return(expr))
                || condition
                    .as_ref()
                    .is_some_and(|expr| always_calls_return(expr))
                || (always_calls_return(loop_body)
                    && else_expr
                        .as_ref()
                        .is_some_and(|expr| always_calls_return(expr)))
        }
        AnalyzedExpressionKind::Declaration { var_name: _, value } => always_calls_return(value),
        AnalyzedExpressionKind::ValueOfAssignable(expr) => always_calls_return_assignable(expr),
        AnalyzedExpressionKind::StructInstance { fields } => {
            fields.iter().any(|(_, value)| always_calls_return(value))
        }
        AnalyzedExpressionKind::Literal(_) => false,
        AnalyzedExpressionKind::Unary { op: _, expr } => always_calls_return(expr),
        AnalyzedExpressionKind::Binary { op, left, right } => match op {
            AnalyzedBinaryOp::Logical(_) => always_calls_return(left),
            _ => always_calls_return(left) || always_calls_return(right),
        },
        AnalyzedExpressionKind::Assign { op, lhs, rhs } => match op {
            BinaryAssignOp::LogicAssign(_) => always_calls_return_assignable(lhs),
            _ => always_calls_return_assignable(lhs) || always_calls_return(rhs),
        },
        AnalyzedExpressionKind::Borrow { expr } => always_calls_return_assignable(expr),
        AnalyzedExpressionKind::FunctionCall { call_type, args } => {
            args.iter().any(always_calls_return)
                || match call_type {
                    AnalyzedFunctionCallType::FunctionPointer(inner) => always_calls_return(inner),
                    AnalyzedFunctionCallType::Function(_) => false,
                }
        }
        AnalyzedExpressionKind::FieldAccess {
            expr,
            field_name: _,
        } => always_calls_return(expr),
        AnalyzedExpressionKind::Increment(inner, _) => always_calls_return_assignable(inner),
        AnalyzedExpressionKind::Decrement(inner, _) => always_calls_return_assignable(inner),
        AnalyzedExpressionKind::ConstantPointer(_) => false,
        AnalyzedExpressionKind::Sizeof(_) => false,
        AnalyzedExpressionKind::FunctionPointer(_) => false,
    }
}

fn always_calls_return_assignable(expression: &AssignableExpression) -> bool {
    match &expression.kind {
        AssignableExpressionKind::LocalVariable(_) => false,
        AssignableExpressionKind::Dereference(expr) => always_calls_return(expr),
        AssignableExpressionKind::FieldAccess(expr, _) => always_calls_return_assignable(expr),
        AssignableExpressionKind::ArrayIndex(arr_expr, index_expr) => {
            always_calls_return(arr_expr) || always_calls_return(index_expr)
        }
        AssignableExpressionKind::PointerFieldAccess(expr, _, _) => always_calls_return(expr),
    }
}
