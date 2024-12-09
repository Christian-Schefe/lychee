use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedBinaryOp, AnalyzedConstant, AnalyzedExpression, AnalyzedExpressionKind,
    AnalyzedLiteral, AnalyzedUnaryOp, AssignableExpression, AssignableExpressionKind,
    BinaryAssignOp,
};
use crate::compiler::analyzer::expression_analyzer::{analyze_assignable_expression, can_cast_to};
use crate::compiler::analyzer::program_analyzer::{AnalyzerContext, LocalVariable};
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::merger::merged_expression::{
    MergedExpression, MergedExpressionKind, MergedLiteral, MergedUnaryOp, TypeId,
};
use crate::compiler::parser::parsed_expression::{BinaryComparisonOp, BinaryOp};
use anyhow::Context;
use std::collections::HashSet;

pub fn analyze_expression(
    context: &mut AnalyzerContext,
    expression: &MergedExpression,
) -> AnalyzerResult<AnalyzedExpression> {
    let mut local_var_stack = vec![];
    let mut stack = vec![(expression, false, false)];
    let mut output: Vec<AnalyzedExpression> = vec![];

    while let Some((stack_expr, was_visited, in_loop)) = stack.pop() {
        let location = stack_expr.location.clone();
        if !was_visited {
            stack.push((stack_expr, true, in_loop));
            match &stack_expr.value {
                MergedExpressionKind::Block { expressions, .. } => {
                    local_var_stack.push(context.local_variables.clone());
                    context
                        .local_variables
                        .values_mut()
                        .for_each(|v| v.is_current_scope = false);
                    for expression in expressions.iter().rev() {
                        stack.push((expression, false, in_loop));
                    }
                }
                MergedExpressionKind::Return(maybe_expr) => {
                    if let Some(expr) = maybe_expr {
                        stack.push((expr, false, in_loop));
                    }
                }
                MergedExpressionKind::Continue => {}
                MergedExpressionKind::Break(maybe_expr) => {
                    if let Some(expr) = maybe_expr {
                        stack.push((expr, false, in_loop));
                    }
                }
                MergedExpressionKind::If {
                    condition,
                    then_block,
                    else_expr,
                } => {
                    if let Some(else_expr) = else_expr {
                        stack.push((else_expr, false, in_loop));
                    }
                    stack.push((then_block, false, in_loop));
                    stack.push((condition, false, in_loop));
                }
                MergedExpressionKind::Loop {
                    init,
                    condition,
                    step,
                    loop_body,
                    else_expr,
                } => {
                    if let Some(else_expr) = else_expr {
                        stack.push((else_expr, false, in_loop));
                    }
                    stack.push((loop_body, false, true));
                    if let Some(step) = step {
                        stack.push((step, false, false));
                    }
                    if let Some(condition) = condition {
                        stack.push((condition, false, false));
                    }
                    if let Some(init) = init {
                        stack.push((init, false, in_loop));
                    }
                }
                MergedExpressionKind::Declaration { value, .. } => {
                    stack.push((value, false, in_loop));
                }
                MergedExpressionKind::Variable(_) => {}
                MergedExpressionKind::Literal(lit) => match lit {
                    MergedLiteral::Struct(ty, fields) => {
                        let struct_decl = context.structs.get(&ty).unwrap();
                        for field in struct_decl.field_order.iter().rev() {
                            stack.push((fields.get(field).unwrap(), false, in_loop));
                        }
                    }
                    _ => {}
                },
                MergedExpressionKind::Unary { op, expr } => {
                    let expr_is_assignable = match op {
                        MergedUnaryOp::Index(index) => {
                            stack.push((index, false, in_loop));
                            false
                        }
                        MergedUnaryOp::Increment { .. }
                        | MergedUnaryOp::Decrement { .. }
                        | MergedUnaryOp::Borrow => true,
                        _ => false,
                    };
                    if !expr_is_assignable {
                        stack.push((expr, false, in_loop));
                    }
                }
                MergedExpressionKind::Binary { left, right, op } => {
                    let left_is_assignable = match op {
                        BinaryOp::Assign | BinaryOp::MathAssign(_) | BinaryOp::LogicAssign(_) => {
                            true
                        }
                        _ => false,
                    };
                    stack.push((right, false, in_loop));
                    if !left_is_assignable {
                        stack.push((left, false, in_loop));
                    }
                }
                MergedExpressionKind::FunctionCall { args, .. } => {
                    for arg in args.iter().rev() {
                        stack.push((arg, false, in_loop));
                    }
                }
            }
        } else {
            let (ty, analyzed) = match &stack_expr.value {
                MergedExpressionKind::Block {
                    expressions,
                    returns_value,
                } => {
                    context.local_variables = local_var_stack.pop().unwrap();
                    let new_len = output.len() - expressions.len();
                    let analyzed_expressions = output.split_off(new_len);
                    let return_ty = analyzed_expressions
                        .last()
                        .filter(|_| *returns_value)
                        .map(|a| a.ty.clone())
                        .unwrap_or(TypeId::Unit);
                    (
                        return_ty,
                        AnalyzedExpressionKind::Block {
                            expressions: analyzed_expressions,
                            returns_value: *returns_value,
                        },
                    )
                }
                MergedExpressionKind::Return(maybe_expr) => {
                    let analyzed_expr =
                        maybe_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let return_ty = analyzed_expr
                        .as_ref()
                        .map_or(TypeId::Unit, |e| e.ty.clone());
                    if return_ty != *context.return_type {
                        Err(anyhow::anyhow!(
                            "Return type '{}' does not match function return type '{}' at {}.",
                            return_ty,
                            context.return_type,
                            location
                        ))?;
                    }
                    (TypeId::Unit, AnalyzedExpressionKind::Return(analyzed_expr))
                }
                MergedExpressionKind::Continue => {
                    if !in_loop {
                        Err(anyhow::anyhow!("Continue outside of loop at {}.", location))?;
                    }
                    (TypeId::Unit, AnalyzedExpressionKind::Continue)
                }
                MergedExpressionKind::Break(maybe_expr) => {
                    if !in_loop {
                        Err(anyhow::anyhow!("Break outside of loop at {}.", location))?;
                    }
                    let analyzed_expr =
                        maybe_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    (TypeId::Unit, AnalyzedExpressionKind::Break(analyzed_expr))
                }
                MergedExpressionKind::If { else_expr, .. } => {
                    let analyzed_else = else_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let analyzed_then = output.pop().unwrap();
                    let analyzed_condition = output.pop().unwrap();
                    if analyzed_condition.ty != TypeId::Bool {
                        Err(anyhow::anyhow!(
                            "If condition has non-bool type at {}.",
                            location
                        ))?;
                    }
                    let return_ty = if let Some(else_expr) = &analyzed_else {
                        if analyzed_then.ty != else_expr.ty {
                            Err(anyhow::anyhow!(
                                "If then block has type '{}', but else block has type '{}' at {}.",
                                analyzed_then.ty,
                                else_expr.ty,
                                location
                            ))?;
                        }
                        analyzed_then.ty.clone()
                    } else {
                        if analyzed_then.ty != TypeId::Unit {
                            Err(anyhow::anyhow!(
                                "If then block has non-unit type '{}' but else block is missing at {}.",
                                analyzed_then.ty,
                                location
                            ))?;
                        }
                        TypeId::Unit
                    };
                    (
                        return_ty,
                        AnalyzedExpressionKind::If {
                            condition: Box::new(analyzed_condition),
                            then_block: Box::new(analyzed_then),
                            else_expr: analyzed_else,
                        },
                    )
                }
                MergedExpressionKind::Loop {
                    init,
                    condition,
                    step,
                    loop_body: _,
                    else_expr,
                } => {
                    let analyzed_else = else_expr.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let analyzed_loop_body = output.pop().unwrap();
                    let analyzed_step = step.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let analyzed_condition =
                        condition.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    let analyzed_init = init.as_ref().map(|_| Box::new(output.pop().unwrap()));
                    if analyzed_init.as_ref().is_some_and(|x| x.ty != TypeId::Unit) {
                        Err(anyhow::anyhow!(
                            "Loop init has non-unit type '{}' at {}.",
                            analyzed_init.as_ref().unwrap().ty,
                            location
                        ))?;
                    }
                    if analyzed_condition
                        .as_ref()
                        .is_some_and(|x| x.ty != TypeId::Bool)
                    {
                        Err(anyhow::anyhow!(
                            "Loop condition has non-bool type '{}' at {}.",
                            analyzed_condition.as_ref().unwrap().ty,
                            location
                        ))?;
                    }
                    match analyzed_step
                        .as_ref()
                        .map(|x| x.ty.clone())
                        .unwrap_or(TypeId::Unit)
                    {
                        TypeId::Unit => {}
                        TypeId::Integer(_) => {}
                        any_ty => Err(anyhow::anyhow!(
                            "Loop step has non-unit/non-integer type '{}' at {}.",
                            any_ty,
                            location
                        ))?,
                    }
                    if analyzed_loop_body.ty != TypeId::Unit {
                        Err(anyhow::anyhow!(
                            "Loop body has non-unit type '{}' at {}.",
                            analyzed_loop_body.ty,
                            location
                        ))?;
                    }
                    let else_ty = analyzed_else.as_ref().map(|e| e.ty.clone());

                    let has_condition = analyzed_condition.is_some();
                    let has_else = analyzed_else.is_some();

                    let required_break_type = match (has_condition, has_else) {
                        (true, true) => Some(else_ty.unwrap()),
                        (true, false) => Some(TypeId::Unit),
                        (false, true) => unreachable!("Else without condition"),
                        (false, false) => None,
                    };

                    let final_return_ty = assert_break_return_type(
                        required_break_type.as_ref(),
                        &analyzed_loop_body,
                    )?
                    .unwrap_or(TypeId::Unit);
                    (
                        final_return_ty,
                        AnalyzedExpressionKind::Loop {
                            init: analyzed_init,
                            condition: analyzed_condition,
                            step: analyzed_step,
                            loop_body: Box::new(analyzed_loop_body),
                            else_expr: analyzed_else,
                        },
                    )
                }
                MergedExpressionKind::Declaration {
                    value,
                    var_type,
                    var_name,
                } => {
                    let analyzed_value = output.pop().unwrap();
                    if let Some(declared_type) = var_type {
                        if analyzed_value.ty != *declared_type {
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
                    (
                        TypeId::Unit,
                        AnalyzedExpressionKind::Declaration {
                            var_name: var_name.clone(),
                            value: Box::new(analyzed_value),
                        },
                    )
                }
                MergedExpressionKind::Variable(var_name) => {
                    let local_var = context.local_variables.get(var_name).ok_or_else(|| {
                        anyhow::anyhow!(
                            "Variable '{}' not declared at {}.",
                            var_name,
                            expression.location
                        )
                    })?;
                    (
                        local_var.ty.clone(),
                        AnalyzedExpressionKind::ValueOfAssignable(AssignableExpression {
                            kind: AssignableExpressionKind::LocalVariable(var_name.clone()),
                            ty: local_var.ty.clone(),
                        }),
                    )
                }
                MergedExpressionKind::Literal(lit) => match lit {
                    MergedLiteral::Unit => (
                        TypeId::Unit,
                        AnalyzedExpressionKind::Literal(AnalyzedLiteral::Unit),
                    ),
                    MergedLiteral::Bool(b) => (
                        TypeId::Bool,
                        AnalyzedExpressionKind::Literal(AnalyzedLiteral::Bool(*b)),
                    ),
                    MergedLiteral::Char(c) => (
                        TypeId::Char,
                        AnalyzedExpressionKind::Literal(AnalyzedLiteral::Char(*c)),
                    ),
                    MergedLiteral::Integer(val) => {
                        let ty = if *val >= -2147483648 && *val <= 2147483647 {
                            TypeId::Integer(4)
                        } else {
                            TypeId::Integer(8)
                        };
                        (
                            ty,
                            AnalyzedExpressionKind::Literal(AnalyzedLiteral::Integer(*val)),
                        )
                    }
                    MergedLiteral::String(val) => {
                        let mut bytes = val.as_bytes().to_vec();
                        bytes.push(0);
                        (
                            TypeId::Pointer(Box::new(TypeId::Char)),
                            AnalyzedExpressionKind::ConstantPointer(AnalyzedConstant::String(
                                bytes,
                            )),
                        )
                    }
                    MergedLiteral::Struct(ty, field_values) => {
                        let struct_type = context.structs.get(ty).ok_or_else(|| {
                            anyhow::anyhow!(
                                "Struct type '{}' not found at {}.",
                                ty,
                                expression.location
                            )
                        })?;

                        let mut present_fields = field_values.keys().collect::<HashSet<_>>();
                        let mut analyzed_field_values = Vec::new();

                        for field_name in struct_type.field_order.iter().rev() {
                            let field_expr = field_values.get(field_name);
                            let field_value = field_expr.ok_or_else(|| {
                                anyhow::anyhow!(
                                    "Struct literal '{}' is missing field '{}' at {}.",
                                    ty,
                                    field_name,
                                    expression.location
                                )
                            })?;
                            let analyzed_field_value = output.pop().unwrap();
                            let expected_type = struct_type.field_types.get(field_name).unwrap();
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

                        analyzed_field_values.reverse();

                        if !present_fields.is_empty() {
                            Err(anyhow::anyhow!(
                                "Struct literal of type '{}' has extra fields: {:?} at {}.",
                                ty,
                                present_fields,
                                expression.location
                            ))?;
                        }

                        (
                            ty.clone(),
                            AnalyzedExpressionKind::Literal(AnalyzedLiteral::Struct(
                                analyzed_field_values,
                            )),
                        )
                    }
                },
                MergedExpressionKind::Unary { expr, op } => match op {
                    MergedUnaryOp::Math(math_op) => {
                        let analyzed_expr = output.pop().unwrap();
                        match analyzed_expr.ty {
                            TypeId::Integer(_) => {}
                            _ => Err(anyhow::anyhow!(
                                "Math unary expression has non-integer type '{}' at {}.",
                                analyzed_expr.ty,
                                expression.location
                            ))?,
                        }

                        (
                            analyzed_expr.ty.clone(),
                            AnalyzedExpressionKind::Unary {
                                op: AnalyzedUnaryOp::Math(math_op.clone()),
                                expr: Box::new(analyzed_expr),
                            },
                        )
                    }
                    MergedUnaryOp::LogicalNot => {
                        let analyzed_expr = output.pop().unwrap();
                        if analyzed_expr.ty != TypeId::Bool {
                            Err(anyhow::anyhow!(
                                "Logical not expression has non-bool type '{}' at {}.",
                                analyzed_expr.ty,
                                expression.location
                            ))?;
                        }

                        (
                            TypeId::Bool,
                            AnalyzedExpressionKind::Unary {
                                op: AnalyzedUnaryOp::LogicalNot,
                                expr: Box::new(analyzed_expr),
                            },
                        )
                    }
                    MergedUnaryOp::Borrow => {
                        let analyzed_expr = analyze_assignable_expression(context, expr)
                            .with_context(|| {
                                format!(
                                    "Failed to analyze type of borrow expression at {}.",
                                    expression.location
                                )
                            })?;
                        (
                            TypeId::Pointer(Box::new(analyzed_expr.ty.clone())),
                            AnalyzedExpressionKind::Borrow {
                                expr: analyzed_expr,
                            },
                        )
                    }
                    MergedUnaryOp::Dereference => {
                        let analyzed_expr = output.pop().unwrap();
                        match analyzed_expr.ty.clone() {
                            TypeId::Pointer(inner) => (
                                *inner.clone(),
                                AnalyzedExpressionKind::ValueOfAssignable(AssignableExpression {
                                    kind: AssignableExpressionKind::Dereference(Box::new(
                                        analyzed_expr,
                                    )),
                                    ty: *inner,
                                }),
                            ),
                            _ => Err(anyhow::anyhow!(
                                "Dereference expression has non-pointer type '{}' at {}.",
                                analyzed_expr.ty,
                                expression.location
                            ))?,
                        }
                    }
                    MergedUnaryOp::Increment { is_prefix } => {
                        let analyzed_expr = analyze_assignable_expression(context, expr)
                            .with_context(|| {
                                format!(
                                    "Failed to analyze type of increment expression at {}.",
                                    expression.location
                                )
                            })?;
                        match &analyzed_expr.ty {
                            TypeId::Integer(_) => {}
                            _ => Err(anyhow::anyhow!(
                                "Increment expression has non-integer type '{}' at {}.",
                                analyzed_expr.ty,
                                expression.location
                            ))?,
                        }
                        (
                            analyzed_expr.ty.clone(),
                            AnalyzedExpressionKind::Increment(analyzed_expr, *is_prefix),
                        )
                    }
                    MergedUnaryOp::Decrement { is_prefix } => {
                        let analyzed_expr = analyze_assignable_expression(context, expr)
                            .with_context(|| {
                                format!(
                                    "Failed to analyze type of decrement expression at {}.",
                                    expression.location
                                )
                            })?;
                        match &analyzed_expr.ty {
                            TypeId::Integer(_) => {}
                            _ => Err(anyhow::anyhow!(
                                "Decrement expression has non-integer type '{}' at {}.",
                                analyzed_expr.ty,
                                expression.location
                            ))?,
                        }
                        (
                            analyzed_expr.ty.clone(),
                            AnalyzedExpressionKind::Decrement(analyzed_expr, *is_prefix),
                        )
                    }
                    MergedUnaryOp::Cast(target_type) => {
                        let analyzed_expr = output.pop().unwrap();
                        if can_cast_to(&analyzed_expr.ty, target_type) {
                            (
                                target_type.clone(),
                                AnalyzedExpressionKind::Unary {
                                    op: AnalyzedUnaryOp::Cast,
                                    expr: Box::new(analyzed_expr),
                                },
                            )
                        } else {
                            Err(anyhow::anyhow!(
                                "Cast expression has type '{}', but expected '{}' at {}.",
                                analyzed_expr.ty,
                                target_type,
                                expression.location
                            ))?
                        }
                    }
                    MergedUnaryOp::Member(member) => {
                        let analyzed_expr = output.pop().unwrap();
                        match &analyzed_expr.ty {
                            TypeId::StructType(_) => {
                                let struct_type =
                                    context.structs.get(&analyzed_expr.ty).ok_or_else(|| {
                                        anyhow::anyhow!(
                                            "Struct type '{}' not found at {}.",
                                            analyzed_expr.ty,
                                            expr.location
                                        )
                                    })?;
                                let field_type =
                                    struct_type.field_types.get(member).ok_or_else(|| {
                                        anyhow::anyhow!(
                                            "Struct type '{}' does not have field '{}' at {}.",
                                            analyzed_expr.ty,
                                            member,
                                            expr.location
                                        )
                                    })?;
                                (
                                    field_type.clone(),
                                    AnalyzedExpressionKind::FieldAccess {
                                        field_name: member.clone(),
                                        expr: Box::new(analyzed_expr),
                                    },
                                )
                            }
                            TypeId::Pointer(inner) => {
                                if let TypeId::StructType(_) = inner.as_ref() {
                                    let struct_type =
                                        context.structs.get(inner).ok_or_else(|| {
                                            anyhow::anyhow!(
                                                "Struct type '{}' not found at {}.",
                                                inner,
                                                expr.location
                                            )
                                        })?;
                                    let field_type =
                                        struct_type.field_types.get(member).ok_or_else(|| {
                                            anyhow::anyhow!(
                                                "Struct type '{}' does not have field '{}' at {}.",
                                                inner,
                                                member,
                                                expr.location
                                            )
                                        })?;

                                    (
                                        field_type.clone(),
                                        AnalyzedExpressionKind::ValueOfAssignable(
                                            AssignableExpression {
                                                kind: AssignableExpressionKind::PointerFieldAccess(
                                                    Box::new(analyzed_expr),
                                                    member.clone(),
                                                ),
                                                ty: field_type.clone(),
                                            },
                                        ),
                                    )
                                } else {
                                    Err(anyhow::anyhow!(
                                        "Expected struct type, found '{}' at {}.",
                                        analyzed_expr.ty,
                                        expr.location
                                    ))?
                                }
                            }
                            _ => Err(anyhow::anyhow!(
                                "Expected struct type, found '{}' at {}.",
                                analyzed_expr.ty,
                                expr.location
                            ))?,
                        }
                    }
                    MergedUnaryOp::Index(index) => {
                        let analyzed_index = output.pop().unwrap();
                        let analyzed_expr = output.pop().unwrap();
                        match &analyzed_index.ty {
                            TypeId::Integer(_) => {}
                            _ => Err(anyhow::anyhow!(
                                "Index expression has non-integer type '{}' at {}.",
                                analyzed_index.ty,
                                index.location
                            ))?,
                        }
                        match analyzed_expr.ty.clone() {
                            TypeId::Pointer(inner) => (
                                *inner.clone(),
                                AnalyzedExpressionKind::ValueOfAssignable(AssignableExpression {
                                    kind: AssignableExpressionKind::ArrayIndex(
                                        Box::new(analyzed_expr),
                                        Box::new(analyzed_index),
                                    ),
                                    ty: *inner,
                                }),
                            ),
                            _ => Err(anyhow::anyhow!(
                                "Index expression has non-array type '{}' at {}.",
                                analyzed_expr.ty,
                                expr.location
                            ))?,
                        }
                    }
                },
                MergedExpressionKind::Binary { left, op, right } => match op {
                    BinaryOp::Math(math_op) => {
                        let analyzed_right = output.pop().unwrap();
                        let analyzed_left = output.pop().unwrap();
                        match &analyzed_left.ty {
                            TypeId::Integer(_) => {}
                            _ => Err(anyhow::anyhow!(
                                "Math binary left expression has non-integer type '{}' at {}.",
                                analyzed_left.ty,
                                left.location
                            ))?,
                        }
                        if analyzed_left.ty != analyzed_right.ty {
                            Err(anyhow::anyhow!("Math binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                        }

                        (
                            analyzed_left.ty.clone(),
                            AnalyzedExpressionKind::Binary {
                                op: AnalyzedBinaryOp::Math(math_op.clone()),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                        )
                    }
                    BinaryOp::Logical(logic_op) => {
                        let analyzed_right = output.pop().unwrap();
                        let analyzed_left = output.pop().unwrap();
                        if analyzed_left.ty != TypeId::Bool {
                            Err(anyhow::anyhow!(
                                "Logic binary left expression has non-bool type '{}' at {}.",
                                analyzed_left.ty,
                                left.location
                            ))?;
                        }
                        if analyzed_right.ty != TypeId::Bool {
                            Err(anyhow::anyhow!(
                                "Logic binary right expression has non-bool type '{}' at {}.",
                                analyzed_right.ty,
                                right.location
                            ))?;
                        }

                        (
                            TypeId::Bool,
                            AnalyzedExpressionKind::Binary {
                                op: AnalyzedBinaryOp::Logical(logic_op.clone()),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                        )
                    }
                    BinaryOp::Comparison(comp_op) => {
                        let analyzed_right = output.pop().unwrap();
                        let analyzed_left = output.pop().unwrap();
                        let needs_integers = match comp_op {
                            BinaryComparisonOp::Equals | BinaryComparisonOp::NotEquals => false,
                            _ => true,
                        };
                        match &analyzed_left.ty {
                            TypeId::Integer(_) => {}
                            TypeId::Char | TypeId::Bool if !needs_integers => {}
                            _ => Err(anyhow::anyhow!(
                            "Comparison binary left expression has non-comparable type '{}' at {}.",
                            analyzed_left.ty,
                            left.location
                        ))?,
                        }
                        if analyzed_left.ty != analyzed_right.ty {
                            Err(anyhow::anyhow!("Comparison binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                        }

                        (
                            TypeId::Bool,
                            AnalyzedExpressionKind::Binary {
                                op: AnalyzedBinaryOp::Comparison(comp_op.clone()),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                        )
                    }
                    BinaryOp::Assign => {
                        let analyzed_right = output.pop().unwrap();
                        let analyzed_left =
                            analyze_assignable_expression(context, left).with_context(|| {
                                format!(
                                    "Failed to analyze type of assign binary left expression at {}.",
                                    expression.location
                                )
                            })?;
                        if analyzed_left.ty != analyzed_right.ty {
                            Err(anyhow::anyhow!("Assign binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                        }
                        (
                            analyzed_left.ty.clone(),
                            AnalyzedExpressionKind::Assign {
                                op: BinaryAssignOp::Assign,
                                lhs: analyzed_left,
                                rhs: Box::new(analyzed_right),
                            },
                        )
                    }
                    BinaryOp::MathAssign(math_op) => {
                        let analyzed_right = output.pop().unwrap();
                        let analyzed_left = analyze_assignable_expression(context, left).with_context(|| format!("Failed to analyze type of math assign binary left expression at {}.", expression.location))?;

                        match &analyzed_left.ty {
                            TypeId::Integer(_) => {}
                            _ => Err(anyhow::anyhow!(
                            "Math assign binary left expression has non-integer type '{}' at {}.",
                            analyzed_left.ty,
                            left.location
                        ))?,
                        }
                        if analyzed_left.ty != analyzed_right.ty {
                            Err(anyhow::anyhow!("Math assign binary left expression has type '{}', but right expression has type '{}' at {}.", analyzed_left.ty, analyzed_right.ty, right.location))?;
                        }

                        (
                            analyzed_left.ty.clone(),
                            AnalyzedExpressionKind::Assign {
                                op: BinaryAssignOp::MathAssign(math_op.clone()),
                                lhs: analyzed_left,
                                rhs: Box::new(analyzed_right),
                            },
                        )
                    }
                    BinaryOp::LogicAssign(logic_op) => {
                        let analyzed_right = output.pop().unwrap();
                        let analyzed_left = analyze_assignable_expression(context, left).with_context(|| format!("Failed to analyze type of logic assign binary left expression at {}.", expression.location))?;
                        if analyzed_left.ty != TypeId::Bool {
                            Err(anyhow::anyhow!(
                                "Logic assign binary left expression has non-bool type '{}' at {}.",
                                analyzed_left.ty,
                                left.location
                            ))?;
                        }
                        if analyzed_right.ty != TypeId::Bool {
                            Err(anyhow::anyhow!(
                            "Logic assign binary right expression has non-bool type '{}' at {}.",
                            analyzed_right.ty,
                            right.location
                        ))?;
                        }

                        (
                            TypeId::Bool,
                            AnalyzedExpressionKind::Assign {
                                op: BinaryAssignOp::LogicAssign(logic_op.clone()),
                                lhs: analyzed_left,
                                rhs: Box::new(analyzed_right),
                            },
                        )
                    }
                },
                MergedExpressionKind::FunctionCall { function_id, args } => {
                    let function_header = context
                        .function_headers
                        .functions
                        .get(function_id)
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Function '{}' not found at {}.",
                                function_id,
                                expression.location
                            )
                        })?;

                    if args.len() != function_header.parameter_order.len() {
                        Err(anyhow::anyhow!(
                            "Function '{}' expects {} arguments, but got {} at {}.",
                            function_id,
                            function_header.parameter_order.len(),
                            args.len(),
                            expression.location
                        ))?;
                    }

                    let mut analyzed_args = Vec::new();
                    for (arg, arg_name) in args
                        .iter()
                        .zip(function_header.parameter_order.iter())
                        .rev()
                    {
                        let analyzed_arg = output.pop().unwrap();
                        let arg_type = function_header.parameter_types.get(arg_name).unwrap();
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

                    analyzed_args.reverse();

                    (
                        function_header.return_type.clone(),
                        AnalyzedExpressionKind::FunctionCall {
                            function_name: function_id.clone(),
                            args: analyzed_args,
                        },
                    )
                }
            };
            output.push(AnalyzedExpression {
                kind: analyzed,
                ty,
                location,
            });
        }
    }

    Ok(output.pop().unwrap())
}

fn assert_break_return_type(
    break_type: Option<&TypeId>,
    expr: &AnalyzedExpression,
) -> AnalyzerResult<Option<TypeId>> {
    match &expr.kind {
        AnalyzedExpressionKind::Block { expressions, .. } => {
            let mut actual_break_type = break_type.cloned();
            for expr in expressions.iter() {
                actual_break_type = assert_break_return_type(actual_break_type.as_ref(), expr)?;
            }
            Ok(actual_break_type)
        }
        AnalyzedExpressionKind::Return(maybe_expr) => {
            let mut actual_break_type = break_type.cloned();
            if let Some(expr) = maybe_expr {
                actual_break_type = assert_break_return_type(actual_break_type.as_ref(), expr)?;
            }
            Ok(actual_break_type)
        }
        AnalyzedExpressionKind::Continue => Ok(break_type.cloned()),
        AnalyzedExpressionKind::Break(maybe_expr) => {
            let expr_type = maybe_expr.as_ref().map_or(TypeId::Unit, |e| e.ty.clone());
            if break_type.is_some_and(|x| *x != expr_type) {
                Err(anyhow::anyhow!(
                    "Break type '{}' does not match loop return type '{}' at {}.",
                    expr_type,
                    break_type.unwrap(),
                    expr.location
                ))
            } else {
                Ok(Some(expr_type))
            }
        }
        AnalyzedExpressionKind::If {
            condition,
            then_block,
            else_expr,
        } => {
            let mut new_break_type = assert_break_return_type(break_type, condition)?;
            new_break_type = assert_break_return_type(new_break_type.as_ref(), then_block)?;
            if let Some(else_expr) = else_expr {
                new_break_type = assert_break_return_type(new_break_type.as_ref(), else_expr)?;
            }
            Ok(new_break_type)
        }
        AnalyzedExpressionKind::Loop {
            init, else_expr, ..
        } => {
            let mut actual_break_type = break_type.cloned();
            if let Some(init) = init {
                actual_break_type = assert_break_return_type(actual_break_type.as_ref(), init)?;
            }
            if let Some(else_expr) = else_expr {
                actual_break_type =
                    assert_break_return_type(actual_break_type.as_ref(), else_expr)?;
            }
            Ok(actual_break_type)
        }
        AnalyzedExpressionKind::Declaration { value, .. } => {
            assert_break_return_type(break_type, value)
        }
        AnalyzedExpressionKind::ValueOfAssignable(assignable) => {
            assert_break_return_type_assignable(break_type, assignable)
        }
        AnalyzedExpressionKind::Literal(lit) => match lit {
            AnalyzedLiteral::Struct(fields) => {
                let mut actual_break_type = break_type.cloned();
                for (_, field) in fields {
                    actual_break_type =
                        assert_break_return_type(actual_break_type.as_ref(), field)?;
                }
                Ok(actual_break_type)
            }
            _ => Ok(break_type.cloned()),
        },
        AnalyzedExpressionKind::ConstantPointer(_) => Ok(break_type.cloned()),
        AnalyzedExpressionKind::Unary { expr, .. } => assert_break_return_type(break_type, expr),
        AnalyzedExpressionKind::Binary { left, right, .. } => {
            let actual_break_type = assert_break_return_type(break_type, left)?;
            assert_break_return_type(actual_break_type.as_ref(), right)
        }
        AnalyzedExpressionKind::Assign { lhs, rhs, .. } => {
            let actual_break_type = assert_break_return_type_assignable(break_type, lhs)?;
            assert_break_return_type(actual_break_type.as_ref(), rhs)
        }
        AnalyzedExpressionKind::Borrow { expr } => {
            assert_break_return_type_assignable(break_type, expr)
        }
        AnalyzedExpressionKind::FunctionCall { args, .. } => {
            let mut actual_break_type = break_type.cloned();
            for arg in args {
                actual_break_type = assert_break_return_type(actual_break_type.as_ref(), arg)?;
            }
            Ok(actual_break_type)
        }
        AnalyzedExpressionKind::FieldAccess { expr, .. } => {
            assert_break_return_type(break_type, expr)
        }
        AnalyzedExpressionKind::Increment(expr, _) => {
            assert_break_return_type_assignable(break_type, expr)
        }
        AnalyzedExpressionKind::Decrement(expr, _) => {
            assert_break_return_type_assignable(break_type, expr)
        }
    }
}

fn assert_break_return_type_assignable(
    break_type: Option<&TypeId>,
    expr: &AssignableExpression,
) -> AnalyzerResult<Option<TypeId>> {
    match &expr.kind {
        AssignableExpressionKind::LocalVariable(_) => Ok(break_type.cloned()),
        AssignableExpressionKind::Dereference(expr) => assert_break_return_type(break_type, expr),
        AssignableExpressionKind::FieldAccess(expr, _) => {
            assert_break_return_type_assignable(break_type, expr)
        }
        AssignableExpressionKind::PointerFieldAccess(expr, _) => {
            assert_break_return_type(break_type, expr)
        }
        AssignableExpressionKind::ArrayIndex(arr, index) => {
            let actual = assert_break_return_type(break_type, arr)?;
            assert_break_return_type(actual.as_ref(), index)
        }
    }
}