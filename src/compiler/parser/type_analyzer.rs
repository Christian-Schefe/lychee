use crate::compiler::lexer::Location;
use crate::compiler::parser::analyzed_syntax_tree::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::parser::parser_error::LocationError;
use crate::compiler::parser::syntax_tree::{
    BinaryOp, Expression, Program, SrcExpression, SrcFunction, SrcStatement, Statement, UnaryOp,
};
use crate::compiler::parser::types::Type;
use std::collections::HashMap;

pub type ValidationResult<T> = Result<T, LocationError>;

struct ValidationContext {
    function_declarations: HashMap<String, FunctionDeclaration>,
    current_function: Option<String>,
    variable_types: HashMap<String, Type>,
}

#[derive(Clone)]
struct FunctionDeclaration {
    return_type: Option<Type>,
    args: Vec<Type>,
}

fn add_builtin_function_declarations(context: &mut ValidationContext) {
    let write_declaration = FunctionDeclaration {
        return_type: Some(Type::Unit),
        args: vec![Type::Char],
    };
    let read_declaration = FunctionDeclaration {
        return_type: Type::Char.into(),
        args: vec![],
    };
    context
        .function_declarations
        .insert("writechar".to_string(), write_declaration);
    context
        .function_declarations
        .insert("readchar".to_string(), read_declaration);
}

pub fn analyze_program(program: &Program) -> ValidationResult<AnalyzedProgram> {
    let mut context = ValidationContext {
        function_declarations: HashMap::new(),
        current_function: None,
        variable_types: HashMap::new(),
    };
    add_builtin_function_declarations(&mut context);
    let main_function = program
        .functions
        .iter()
        .find(|f| f.function.name == "main")
        .ok_or(LocationError::msg(
            "Program requires a main function.",
            &Location { line: 1, column: 1 },
        ))?;
    if main_function
        .function
        .return_type
        .as_ref()
        .is_some_and(|x| *x != Type::Int)
    {
        return Err(LocationError::msg(
            "Main function must return void or an integer.",
            &main_function.location,
        ));
    }
    for function in &program.functions {
        let declaration = FunctionDeclaration {
            return_type: function.function.return_type.clone(),
            args: function
                .function
                .args
                .iter()
                .map(|(_, ty)| ty.clone())
                .collect(),
        };
        if context
            .function_declarations
            .contains_key(&function.function.name)
        {
            return Err(LocationError::msg(
                &format!(
                    "Function '{}' has already been declared.",
                    function.function.name
                ),
                &function.location,
            ));
        }
        context
            .function_declarations
            .insert(function.function.name.clone(), declaration);
    }
    let mut analyzed_functions = HashMap::new();
    for function in &program.functions {
        let analyzed_fn = analyze_function(&mut context, function)?;
        analyzed_functions.insert(function.function.name.clone(), analyzed_fn);
    }
    Ok(AnalyzedProgram {
        functions: analyzed_functions,
        main_function: main_function.function.name.clone(),
    })
}

fn analyze_function(
    context: &mut ValidationContext,
    function: &SrcFunction,
) -> ValidationResult<AnalyzedFunction> {
    context.current_function = Some(function.function.name.clone());
    context.variable_types = HashMap::new();
    for (name, ty) in &function.function.args {
        context.variable_types.insert(name.clone(), ty.clone());
    }
    let return_type = function.function.return_type.clone().unwrap_or(Type::Unit);
    analyze_expr(context, &function.function.expr, Some(&return_type))?;

    let stack_space = calc_local_var_stack_space_expr(&function.function.expr);
    Ok(AnalyzedFunction {
        name: function.function.name.clone(),
        args: function.function.args.clone(),
        return_type: function.function.return_type.clone(),
        local_var_stack_size: stack_space,
        expr: function.function.expr.clone(),
    })
}

fn analyze_statement(
    context: &mut ValidationContext,
    statement: &SrcStatement,
) -> ValidationResult<()> {
    match &statement.statement {
        Statement::Return(expr) => {
            let function = context
                .function_declarations
                .get(context.current_function.as_ref().unwrap())
                .unwrap();
            if let Some(return_type) = function.return_type.clone() {
                let expr = expr.as_ref().ok_or(LocationError::msg(
                    &format!(
                        "Return statement needs to return a value of type '{:?}'.",
                        &return_type
                    ),
                    &statement.location,
                ))?;
                analyze_expr(context, expr, Some(&return_type))?;
            } else {
                if let Some(src_expr) = expr {
                    return Err(LocationError::msg(
                        "Return statement cannot return a value.",
                        &src_expr.location,
                    ));
                }
            }
        }
        Statement::Declaration {
            var_type,
            value,
            name,
        } => {
            if context.variable_types.contains_key(name) {
                return Err(LocationError::msg(
                    &format!("Variable '{}' has already been declared.", name),
                    &statement.location,
                ));
            }
            analyze_expr(context, value, Some(var_type))?;
            context
                .variable_types
                .insert(name.clone(), var_type.clone());
        }
        Statement::Expr(expr) => {
            analyze_expr(context, expr, None)?;
        }
        Statement::If {
            condition,
            true_expr,
            false_expr,
        } => {
            analyze_expr(context, condition, Some(&Type::Bool))?;
            analyze_expr(context, true_expr, None)?;
            if let Some(false_expr) = false_expr {
                analyze_expr(context, false_expr, None)?;
            }
        }
    }
    Ok(())
}

fn analyze_expr(
    context: &mut ValidationContext,
    expr: &SrcExpression,
    expected_type: Option<&Type>,
) -> ValidationResult<Type> {
    match &expr.expr {
        Expression::Block(statements, return_expr) => {
            let mut always_returns = false;
            for statement in statements {
                if always_returns {
                    return Err(LocationError::msg("Unreachable code.", &statement.location));
                }
                if always_returns_statement(statement) {
                    always_returns = true;
                }
                analyze_statement(context, statement)?;
            }
            if let Some(return_expr) = return_expr {
                if always_returns {
                    return Err(LocationError::msg(
                        "Unreachable code.",
                        &return_expr.location,
                    ));
                }
                analyze_expr(context, return_expr, expected_type)
            } else {
                if expected_type.is_none_or(|x| *x == Type::Unit) {
                    Ok(Type::Unit)
                } else {
                    Err(LocationError::msg(
                        &format!(
                            "Expected type '{:?}', but block doesn't return anything.",
                            expected_type.unwrap()
                        ),
                        &expr.location,
                    ))
                }
            }
        }
        Expression::Literal(literal) => {
            if expected_type.is_none_or(|x| *x == literal.get_type()) {
                Ok(literal.get_type())
            } else {
                Err(LocationError::msg(
                    &format!(
                        "Expected type '{:?}', found type '{:?}'.",
                        expected_type.unwrap(),
                        literal.get_type()
                    ),
                    &expr.location,
                ))
            }
        }
        Expression::Unary {
            op,
            expr: inner_expr,
        } => match op {
            UnaryOp::Positive | UnaryOp::Negate | UnaryOp::Not => {
                if expected_type.is_none_or(|x| x.is_integer()) {
                    let expr_type = analyze_expr(context, inner_expr, expected_type)?;
                    if expr_type.is_integer() {
                        Ok(expr_type)
                    } else {
                        Err(LocationError::msg(
                            &format!(
                                "Unary operation requires an integer type. Found type '{:?}'.",
                                &expr_type
                            ),
                            &inner_expr.location,
                        ))
                    }
                } else {
                    Err(LocationError::msg(
                        &format!(
                            "Unary operation yields an integer type. Found type '{:?}'.",
                            expected_type
                        ),
                        &expr.location,
                    ))
                }
            }
            UnaryOp::LogicalNot => {
                if expected_type.is_none_or(|x| *x == Type::Bool) {
                    analyze_expr(context, inner_expr, Some(&Type::Bool))
                } else {
                    Err(LocationError::msg(
                        &format!(
                            "Unary operation yields a boolean type. Found type '{:?}'.",
                            expected_type
                        ),
                        &inner_expr.location,
                    ))
                }
            }
        },
        Expression::Binary { op, left, right } => {
            match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::And
                | BinaryOp::Or
                | BinaryOp::Xor
                | BinaryOp::Shl
                | BinaryOp::Shr => {
                    if expected_type.is_none_or(|x| x.is_integer()) {
                        let left_type = analyze_expr(context, left, expected_type)?;
                        if !left_type.is_integer() {
                            return Err(LocationError::msg(
                                &format!(
                                    "Binary operation requires an integer type. Found type '{:?}'.",
                                    &left_type
                                ),
                                &left.location,
                            ));
                        }
                        analyze_expr(context, right, Some(&left_type))
                    } else {
                        Err(LocationError::msg(
                            &format!(
                                "Binary operation yields an integer type. Found type '{:?}'.",
                                &expected_type
                            ),
                            &expr.location,
                        ))
                    }
                }
                BinaryOp::Less
                | BinaryOp::LessEquals
                | BinaryOp::Greater
                | BinaryOp::GreaterEquals => {
                    if expected_type.is_none_or(|x| *x == Type::Bool) {
                        let left_type = analyze_expr(context, left, None)?;
                        if !left_type.is_integer() {
                            return Err(LocationError::msg(&format!("Comparison operation requires an integer type. Found type '{:?}'.", &left_type), &left.location));
                        }
                        analyze_expr(context, right, Some(&left_type))?;
                        Ok(Type::Bool)
                    } else {
                        Err(LocationError::msg(
                            &format!(
                                "Comparison operation yields a boolean type. Found type '{:?}'.",
                                expected_type
                            ),
                            &expr.location,
                        ))
                    }
                }
                BinaryOp::Equals | BinaryOp::NotEquals => {
                    if expected_type.is_none_or(|x| *x == Type::Bool) {
                        let left_type = analyze_expr(context, left, None)?;
                        analyze_expr(context, right, Some(&left_type))?;
                        Ok(Type::Bool)
                    } else {
                        Err(LocationError::msg(
                            &format!(
                                "Comparison operation yields a boolean type. Found type '{:?}'.",
                                expected_type
                            ),
                            &expr.location,
                        ))
                    }
                }
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                    if expected_type.is_none_or(|x| *x == Type::Bool) {
                        analyze_expr(context, left, Some(&Type::Bool))?;
                        analyze_expr(context, right, Some(&Type::Bool))
                    } else {
                        Err(LocationError::msg(
                            &format!(
                                "Binary operation yields a boolean type. Found type '{:?}'.",
                                expected_type
                            ),
                            &expr.location,
                        ))
                    }
                }
                BinaryOp::Assign => {
                    if is_assignable(&left.expr) {
                        let var_type = analyze_expr(context, right, expected_type)?;
                        analyze_expr(context, left, Some(&var_type))?;
                        Ok(var_type)
                    } else {
                        Err(LocationError::msg(
                            "Expected assignable expression",
                            &expr.location,
                        ))
                    }
                }
                BinaryOp::AddAssign
                | BinaryOp::SubAssign
                | BinaryOp::MulAssign
                | BinaryOp::DivAssign
                | BinaryOp::ModAssign
                | BinaryOp::AndAssign
                | BinaryOp::OrAssign
                | BinaryOp::XorAssign
                | BinaryOp::ShlAssign
                | BinaryOp::ShrAssign => {
                    if is_assignable(&left.expr) {
                        let var_type = analyze_expr(context, left, expected_type)?;
                        if !var_type.is_integer() {
                            return Err(LocationError::msg(&format!("Binary assignment operation requires an integer type. Found type '{:?}'.", &var_type), &left.location));
                        }
                        analyze_expr(context, right, Some(&var_type))?;
                        Ok(var_type)
                    } else {
                        Err(LocationError::msg(
                            "Expected assignable expression",
                            &expr.location,
                        ))
                    }
                }
            }
        }
        Expression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            if expected_type.is_none_or(|x| *x == Type::Bool) {
                analyze_expr(context, condition, Some(&Type::Bool))?;
                let true_type = analyze_expr(context, true_expr, None)?;
                analyze_expr(context, false_expr, Some(&true_type))?;
                Ok(Type::Bool)
            } else {
                Err(LocationError::msg(
                    &format!(
                        "Ternary operation yields a boolean type. Found type '{:?}'.",
                        expected_type
                    ),
                    &expr.location,
                ))
            }
        }
        Expression::Variable(name) => {
            let var_type = context
                .variable_types
                .get(name)
                .cloned()
                .ok_or(LocationError::msg(
                    &format!("Variable '{}' has not been declared.", name),
                    &expr.location,
                ))?;
            if let Some(expected_type) = expected_type {
                if var_type != *expected_type {
                    return Err(LocationError::msg(
                        &format!(
                            "Expected type '{:?}', found type '{:?}'.",
                            expected_type, var_type
                        ),
                        &expr.location,
                    ));
                }
            }
            Ok(var_type)
        }
        Expression::FunctionCall { function, args } => {
            let function = context
                .function_declarations
                .get(function)
                .ok_or(LocationError::msg(
                    &format!("Function '{}' has not been declared.", function),
                    &expr.location,
                ))?
                .clone();
            if function.args.len() != args.len() {
                return Err(LocationError::msg(
                    &format!(
                        "Expected {} arguments, found {}",
                        function.args.len(),
                        args.len()
                    ),
                    &expr.location,
                ));
            }
            let return_type = function.return_type.unwrap_or(Type::Unit);
            if let Some(expected_type) = expected_type {
                if return_type != *expected_type {
                    return Err(LocationError::msg(
                        &format!(
                            "Expected type '{:?}', found type '{:?}'.",
                            expected_type, return_type
                        ),
                        &expr.location,
                    ));
                }
            }
            for (arg, expected_arg_type) in args.iter().zip(function.args.iter()) {
                analyze_expr(context, arg, Some(expected_arg_type))?;
            }
            Ok(return_type)
        }
        Expression::Cast { var_type, expr } => {
            if let Some(expected_type) = expected_type {
                if *var_type != *expected_type {
                    return Err(LocationError::msg(
                        &format!(
                            "Expected type '{:?}', found type '{:?}'.",
                            expected_type, var_type
                        ),
                        &expr.location,
                    ));
                }
            }
            let expr_type = analyze_expr(context, expr, None)?;
            if expr_type.can_cast_to(var_type) {
                Ok(var_type.clone())
            } else {
                Err(LocationError::msg(
                    &format!(
                        "Cannot cast type '{:?}' to type '{:?}'",
                        &expr_type, &var_type
                    ),
                    &expr.location,
                ))
            }
        }
    }
}

fn is_assignable(expr: &Expression) -> bool {
    match expr {
        Expression::Variable(_) => true,
        _ => false,
    }
}

fn always_returns_expr(expr: &SrcExpression) -> bool {
    match &expr.expr {
        Expression::Block(statements, return_expr) => {
            if statements.iter().any(always_returns_statement) {
                return true;
            }
            return_expr
                .as_ref()
                .map(|x| always_returns_expr(x))
                .unwrap_or(false)
        }
        Expression::Binary { op, left, right } => match op {
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => always_returns_expr(left),
            _ => always_returns_expr(left) || always_returns_expr(right),
        },
        Expression::Unary { op: _, expr } => always_returns_expr(expr),
        Expression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            always_returns_expr(condition)
                || (always_returns_expr(true_expr) && always_returns_expr(false_expr))
        }
        Expression::FunctionCall { function: _, args } => args.iter().any(always_returns_expr),
        Expression::Cast { var_type: _, expr } => always_returns_expr(expr),
        Expression::Variable(_) => false,
        Expression::Literal(_) => false,
    }
}

fn always_returns_statement(statement: &SrcStatement) -> bool {
    match &statement.statement {
        Statement::Return(_) => true,
        Statement::Declaration { value, .. } => always_returns_expr(value),
        Statement::Expr(expr) => always_returns_expr(expr),
        Statement::If {
            true_expr,
            false_expr,
            ..
        } => {
            always_returns_expr(true_expr)
                && false_expr
                    .as_ref()
                    .map(always_returns_expr)
                    .unwrap_or(false)
        }
    }
}

fn calc_local_var_stack_space_expr(expression: &SrcExpression) -> usize {
    match &expression.expr {
        Expression::Block(statements, return_expr) => {
            let mut stack_space = 0;
            for statement in statements {
                stack_space += calc_local_var_stack_space_statement(statement);
            }
            if let Some(return_expr) = return_expr {
                stack_space += calc_local_var_stack_space_expr(return_expr);
            }
            stack_space
        }
        Expression::Literal(_) => 0,
        Expression::Unary { op: _, expr } => calc_local_var_stack_space_expr(expr),
        Expression::Binary { op: _, left, right } => {
            calc_local_var_stack_space_expr(left) + calc_local_var_stack_space_expr(right)
        }
        Expression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            calc_local_var_stack_space_expr(condition)
                + calc_local_var_stack_space_expr(true_expr)
                + calc_local_var_stack_space_expr(false_expr)
        }
        Expression::Variable(_) => 0,
        Expression::FunctionCall { function: _, args } => {
            args.iter().map(calc_local_var_stack_space_expr).sum()
        }
        Expression::Cast { var_type: _, expr } => calc_local_var_stack_space_expr(expr),
    }
}

fn calc_local_var_stack_space_statement(statement: &SrcStatement) -> usize {
    match &statement.statement {
        Statement::Return(expr) => expr
            .as_ref()
            .map(calc_local_var_stack_space_expr)
            .unwrap_or(0),
        Statement::Declaration { var_type, .. } => var_type.size(),
        Statement::Expr(expr) => calc_local_var_stack_space_expr(expr),
        Statement::If {
            condition,
            true_expr,
            false_expr,
        } => {
            calc_local_var_stack_space_expr(condition)
                + calc_local_var_stack_space_expr(true_expr)
                + false_expr
                    .as_ref()
                    .map(calc_local_var_stack_space_expr)
                    .unwrap_or(0)
        }
    }
}
