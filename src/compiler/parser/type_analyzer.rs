use std::collections::HashMap;
use crate::compiler::lexer::Location;
use crate::compiler::parser::analyzed_syntax_tree::{AnalyzedFunction, AnalyzedProgram};
use crate::compiler::parser::parser_error::LocationError;
use crate::compiler::parser::syntax_tree::{BinaryOp, Expression, Program, SrcExpression, SrcFunction, SrcStatement, Statement, Type, UnaryOp};

pub type ValidationResult<T> = Result<T, LocationError>;

struct ValidationContext {
    function_return_types: HashMap<String, FunctionDeclaration>,
    current_function: Option<String>,
    variable_types: HashMap<String, Type>,
}

#[derive(Clone)]
struct FunctionDeclaration {
    return_type: Option<Type>,
    args: Vec<Type>,
}

pub fn analyze_program(program: &Program) -> ValidationResult<AnalyzedProgram> {
    let mut context = ValidationContext {
        function_return_types: HashMap::new(),
        current_function: None,
        variable_types: HashMap::new(),
    };
    let main_function = program.functions.iter().find(|f| f.function.name == "main").ok_or(LocationError::msg("Program requires a main function.", &Location { line: 1, column: 1 }))?;
    if main_function.function.return_type.as_ref().is_some_and(|x| *x != Type::Int) {
        return Err(LocationError::msg("Main function must return void or an integer.", &main_function.location));
    }
    for function in &program.functions {
        let declaration = FunctionDeclaration {
            return_type: function.function.return_type.clone(),
            args: function.function.args.iter().map(|(_, ty)| ty.clone()).collect(),
        };
        context.function_return_types.insert(function.function.name.clone(), declaration);
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

fn analyze_function(context: &mut ValidationContext, function: &SrcFunction) -> ValidationResult<AnalyzedFunction> {
    context.current_function = Some(function.function.name.clone());
    context.variable_types = HashMap::new();
    for (name, ty) in &function.function.args {
        context.variable_types.insert(name.clone(), ty.clone());
    }
    let expr_type = analyze_expr(context, &function.function.expr)?;
    let return_type = function.function.return_type.clone().unwrap_or(Type::Unit);
    if expr_type != return_type {
        if !always_returns_expr(&function.function.expr) {
            return Err(LocationError::msg(&format!("Function '{}' must return type '{:?}'. Found type '{:?}'.", function.function.name, return_type, &expr_type), &function.function.expr.location));
        }
    }
    let mut stack_space = calc_local_var_stack_space_expr(&function.function.expr);
    Ok(AnalyzedFunction {
        name: function.function.name.clone(),
        args: function.function.args.clone(),
        return_type: function.function.return_type.clone(),
        local_var_stack_size: stack_space,
        expr: function.function.expr.clone(),
    })
}

fn analyze_statement(context: &mut ValidationContext, statement: &SrcStatement) -> ValidationResult<()> {
    match &statement.statement {
        Statement::Return(expr) => {
            let function = context.function_return_types.get(context.current_function.as_ref().unwrap()).unwrap();
            if let Some(return_type) = function.return_type.clone() {
                let expr = expr.as_ref().ok_or(LocationError::msg(&format!("Return statement needs to return a value of type '{:?}'.", &return_type), &statement.location))?;
                let expr_type = analyze_expr(context, expr)?;
                if expr_type != return_type {
                    return Err(LocationError::msg(&format!("Return statement needs to return a value of type '{:?}'. Found type '{:?}'.", &return_type, &expr_type), &expr.location));
                }
            } else {
                if let Some(src_expr) = expr {
                    return Err(LocationError::msg("Return statement cannot return a value.", &src_expr.location));
                }
            }
        }
        Statement::Declaration { var_type, value, name } => {
            if context.variable_types.contains_key(name) {
                return Err(LocationError::msg(&format!("Variable '{}' has already been declared.", name), &statement.location));
            }
            if let Some(value) = value {
                let value_type = analyze_expr(context, value)?;
                if var_type != &value_type {
                    return Err(LocationError::expect_at(var_type, &value_type, &value.location));
                }
            }
            context.variable_types.insert(name.clone(), var_type.clone());
        }
        Statement::Expr(expr) => {
            analyze_expr(context, expr)?;
        }
        Statement::If { condition, true_expr, false_expr } => {
            let condition_type = analyze_expr(context, condition)?;
            if condition_type != Type::Bool {
                return Err(LocationError::msg(&format!("If condition must be of type '{:?}'. Found type '{:?}'.", Type::Bool, &condition_type), &condition.location));
            }
            analyze_expr(context, true_expr)?;
            if let Some(false_expr) = false_expr {
                analyze_expr(context, false_expr)?;
            }
        }
    }
    Ok(())
}

fn analyze_expr(context: &mut ValidationContext, expr: &SrcExpression) -> ValidationResult<Type> {
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
                    return Err(LocationError::msg("Unreachable code.", &return_expr.location));
                }
                let return_expr_type = analyze_expr(context, return_expr)?;
                Ok(return_expr_type)
            } else {
                Ok(Type::Unit)
            }
        }
        Expression::Literal(literal) => Ok(literal.get_type()),
        Expression::Unary { op, expr } => {
            let expr_type = analyze_expr(context, expr)?;
            match op {
                UnaryOp::Positive | UnaryOp::Negate | UnaryOp::LogicalNot | UnaryOp::Not => {
                    if expr_type.is_integer() {
                        Ok(expr_type)
                    } else {
                        Err(LocationError::expect_at("integer", &expr_type, &expr.location))
                    }
                }
            }
        }
        Expression::Binary { op, left, right } => {
            let left_type = analyze_expr(context, left)?;
            let right_type = analyze_expr(context, right)?;
            match op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::Shl | BinaryOp::Shr => {
                    if left_type.is_integer() && left_type == right_type {
                        Ok(left_type)
                    } else {
                        Err(LocationError::expect2("same type integers", &left_type, &right_type, &expr.location))
                    }
                }
                BinaryOp::Less | BinaryOp::LessEquals | BinaryOp::Greater | BinaryOp::GreaterEquals => {
                    if left_type.is_integer() && right_type.is_integer() {
                        Ok(Type::Bool)
                    } else {
                        Err(LocationError::expect2("two integers", &left_type, &right_type, &expr.location))
                    }
                }
                BinaryOp::Equals | BinaryOp::NotEquals => {
                    if left_type.is_integer() && right_type.is_integer() {
                        Ok(Type::Bool)
                    } else if left_type == right_type {
                        Ok(Type::Bool)
                    } else {
                        Err(LocationError::expect2("two integers or same type", &left_type, &right_type, &expr.location))
                    }
                }
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                    if left_type != Type::Bool {
                        Err(LocationError::msg(&format!("Logical operation requires type '{:?}'. Found type '{:?}'.", Type::Bool, &left_type), &left.location))
                    } else if right_type != Type::Bool {
                        Err(LocationError::msg(&format!("Logical operation requires type '{:?}'. Found type '{:?}'.", Type::Bool, &right_type), &right.location))
                    } else {
                        Ok(Type::Bool)
                    }
                }
                BinaryOp::Assign => {
                    if is_assignable(&left.expr) {
                        if left_type == right_type {
                            Ok(left_type)
                        } else {
                            Err(LocationError::expect_at("same type", &left_type, &expr.location))
                        }
                    } else {
                        Err(LocationError::expect_at("assignable", &left_type, &expr.location))
                    }
                }
                BinaryOp::AddAssign | BinaryOp::SubAssign | BinaryOp::MulAssign | BinaryOp::DivAssign | BinaryOp::ModAssign | BinaryOp::AndAssign | BinaryOp::OrAssign | BinaryOp::XorAssign | BinaryOp::ShlAssign | BinaryOp::ShrAssign => {
                    if is_assignable(&left.expr) {
                        if left_type.is_integer() && left_type == right_type {
                            Ok(left_type)
                        } else {
                            Err(LocationError::expect2("same type integers", &left_type, &right_type, &expr.location))
                        }
                    } else {
                        Err(LocationError::expect_at("assignable", &left_type, &expr.location))
                    }
                }
            }
        }
        Expression::Ternary { condition, true_expr, false_expr } => {
            let condition_type = analyze_expr(context, condition)?;
            if condition_type == Type::Bool {
                let true_type = analyze_expr(context, true_expr)?;
                let false_type = analyze_expr(context, false_expr)?;
                if true_type == false_type {
                    Ok(true_type)
                } else {
                    Err(LocationError::msg(&format!("Ternary true and false expressions must have the same type. Found type '{:?}' at {} and type '{:?}' at {}", &true_type, &true_expr.location, &false_type, &false_expr.location), &expr.location))
                }
            } else {
                Err(LocationError::msg(&format!("Ternary condition must be of type '{:?}'. Found type '{:?}'", Type::Bool, &condition_type), &expr.location))
            }
        }
        Expression::Variable(name) => {
            let var_type = context.variable_types.get(name).cloned().ok_or(LocationError::msg(&format!("Variable '{}' has not been declared.", name), &expr.location))?;
            Ok(var_type)
        }
        Expression::FunctionCall { function, args } => {
            let function = context.function_return_types.get(function).ok_or(LocationError::msg(&format!("Function '{}' has not been declared.", function), &expr.location))?.clone();
            if function.args.len() != args.len() {
                return Err(LocationError::msg(&format!("Expected {} arguments, found {}", function.args.len(), args.len()), &expr.location));
            }
            for (arg, expected_type) in args.iter().zip(function.args.iter()) {
                let arg_type = analyze_expr(context, arg)?;
                if arg_type != *expected_type {
                    return Err(LocationError::msg(&format!("Expected argument of type '{:?}', found type '{:?}'", expected_type, &arg_type), &arg.location));
                }
            }
            Ok(function.return_type.unwrap_or(Type::Unit))
        }
        Expression::Cast { var_type, expr } => {
            let expr_type = analyze_expr(context, expr)?;
            if expr_type.can_cast_to(var_type) {
                Ok(var_type.clone())
            } else {
                Err(LocationError::msg(&format!("Cannot cast type '{:?}' to type '{:?}'", &expr_type, &var_type), &expr.location))
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
            return_expr.as_ref().map(|x| always_returns_expr(x)).unwrap_or(false)
        }
        Expression::Binary { op, left, right } => {
            match op {
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd => always_returns_expr(left),
                _ => always_returns_expr(left) || always_returns_expr(right),
            }
        }
        Expression::Unary { op: _, expr } => always_returns_expr(expr),
        Expression::Ternary { condition, true_expr, false_expr } => always_returns_expr(condition) || (always_returns_expr(true_expr) && always_returns_expr(false_expr)),
        Expression::FunctionCall { function: _, args } => args.iter().any(always_returns_expr),
        Expression::Cast { var_type: _, expr } => always_returns_expr(expr),
        Expression::Variable(_) => false,
        Expression::Literal(_) => false,
    }
}

fn always_returns_statement(statement: &SrcStatement) -> bool {
    match &statement.statement {
        Statement::Return(_) => true,
        Statement::Declaration { value, .. } => value.as_ref().map(always_returns_expr).unwrap_or(false),
        Statement::Expr(expr) => always_returns_expr(expr),
        Statement::If { true_expr, false_expr, .. } => always_returns_expr(true_expr) && false_expr.as_ref().map(always_returns_expr).unwrap_or(false),
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
        Expression::Binary { op: _, left, right } => calc_local_var_stack_space_expr(left) + calc_local_var_stack_space_expr(right),
        Expression::Ternary { condition, true_expr, false_expr } => calc_local_var_stack_space_expr(condition) + calc_local_var_stack_space_expr(true_expr) + calc_local_var_stack_space_expr(false_expr),
        Expression::Variable(_) => 0,
        Expression::FunctionCall { function: _, args } => args.iter().map(calc_local_var_stack_space_expr).sum(),
        Expression::Cast { var_type: _, expr } => calc_local_var_stack_space_expr(expr),
    }
}

fn calc_local_var_stack_space_statement(statement: &SrcStatement) -> usize {
    match &statement.statement {
        Statement::Return(expr) => expr.as_ref().map(calc_local_var_stack_space_expr).unwrap_or(0),
        Statement::Declaration { var_type, .. } => var_type.size(),
        Statement::Expr(expr) => calc_local_var_stack_space_expr(expr),
        Statement::If { condition, true_expr, false_expr } => calc_local_var_stack_space_expr(condition) + calc_local_var_stack_space_expr(true_expr) + false_expr.as_ref().map(calc_local_var_stack_space_expr).unwrap_or(0),
    }
}