use std::collections::HashMap;
use crate::lexer::Location;
use crate::parser::analyzed_syntax_tree::{AnalyzedFunction, AnalyzedProgram};
use crate::parser::parser_error::LocationError;
use crate::parser::syntax_tree::{BinaryOp, Expression, Program, SrcExpression, SrcFunction, SrcStatement, Statement, Type, UnaryOp};

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
    let main_function = program.functions.iter().find(|f| f.function.name == "main").ok_or(LocationError::expect_at("main function", "no main function", &Location { line: 1, column: 1 }))?;
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
    let mut has_return = false;
    for statement in &function.function.statements {
        if has_return {
            return Err(LocationError::expect_at("no statements after return", "statement", &statement.location));
        }
        if let Statement::Return(_) = &statement.statement {
            has_return = true;
        }
        analyze_statement(context, statement)?;
    }
    if !has_return {
        return Err(LocationError::expect_at("return statement", "no return statement", &function.location));
    }
    let mut stack_space = 0;
    for (_, ty) in &context.variable_types {
        stack_space += ty.size();
    }
    Ok(AnalyzedFunction {
        name: function.function.name.clone(),
        args: function.function.args.clone(),
        return_type: function.function.return_type.clone(),
        local_var_stack_size: stack_space,
        statements: function.function.statements.clone(),
    })
}

fn analyze_statement(context: &mut ValidationContext, statement: &SrcStatement) -> ValidationResult<()> {
    match &statement.statement {
        Statement::Return(expr) => {
            let function = context.function_return_types.get(context.current_function.as_ref().unwrap()).unwrap();
            if let Some(return_type) = function.return_type.clone() {
                let expr = expr.as_ref().ok_or(LocationError::expect_at("return expr", "none", &statement.location))?;
                let expr_type = analyze_expr(context, expr)?;
                if expr_type != return_type {
                    return Err(LocationError::expect_at(return_type, &expr_type, &statement.location));
                }
            } else {
                if let Some(src_expr) = expr {
                    return Err(LocationError::expect_at("none", "return expr", &src_expr.location));
                }
            }
        }
        Statement::Declaration { var_type, value, name } => {
            if context.variable_types.contains_key(name) {
                return Err(LocationError::expect_at("unused/shadow-able identifier", "used identifier", &statement.location));
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
    }
    Ok(())
}

fn analyze_expr(context: &mut ValidationContext, expr: &SrcExpression) -> ValidationResult<Type> {
    match &expr.expr {
        Expression::Block(statements, return_expr) => {
            for statement in statements {
                analyze_statement(context, statement)?;
            }
            let return_expr = return_expr.as_ref().map(|expr| analyze_expr(context, expr.as_ref())).transpose()?;
            Ok(return_expr.unwrap_or(Type::Unit))
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
                    if left_type == Type::Bool && right_type == Type::Bool {
                        Ok(Type::Bool)
                    } else {
                        Err(LocationError::expect2("two booleans", &left_type, &right_type, &expr.location))
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
                    Err(LocationError::expect2("same type", &true_type, &false_type, &expr.location))
                }
            } else {
                Err(LocationError::expect_at("boolean", &condition_type, &expr.location))
            }
        }
        Expression::Variable(name) => {
            let var_type = context.variable_types.get(name).cloned().ok_or(LocationError::expect_at("declared variable", "unknown variable", &expr.location))?;
            Ok(var_type)
        }
        Expression::FunctionCall { function, args } => {
            let function = context.function_return_types.get(function).ok_or(LocationError::expect_at("declared function", "unknown function", &expr.location))?.clone();
            if function.args.len() != args.len() {
                return Err(LocationError::expect2("argument count", &function.args.len(), &args.len(), &expr.location));
            }
            for (arg, expected_type) in args.iter().zip(function.args.iter()) {
                let arg_type = analyze_expr(context, arg)?;
                if arg_type != *expected_type {
                    return Err(LocationError::expect_at(expected_type, &arg_type, &arg.location));
                }
            }
            Ok(function.return_type.unwrap_or(Type::Unit))
        }
    }
}

pub fn is_assignable(expr: &Expression) -> bool {
    match expr {
        Expression::Variable(_) => true,
        _ => false,
    }
}
