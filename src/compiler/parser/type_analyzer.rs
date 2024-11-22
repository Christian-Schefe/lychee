use crate::compiler::parser::syntax_tree::Literal;
use crate::compiler::lexer::Location;
use crate::compiler::parser::analyzed_syntax_tree::{AnalyzedExpression, AnalyzedFunction, AnalyzedProgram, AnalyzedStatement, TypedAnalyzedExpression};
use crate::compiler::parser::parser_error::LocationError;
use crate::compiler::parser::syntax_tree::{BinaryOp, BinaryComparisonOp, Expression, Program, SrcExpression, SrcFunction, SrcStatement, Statement, UnaryOp};
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
    return_type: Type,
    args: Vec<Type>,
}

fn add_builtin_function_declarations(context: &mut ValidationContext) {
    let write_declaration = FunctionDeclaration {
        return_type: Type::Unit,
        args: vec![Type::Char],
    };
    let read_declaration = FunctionDeclaration {
        return_type: Type::Char,
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
    if main_function.function.return_type != Type::Unit && !matches!(main_function.function.return_type, Type::Integer { size: _ }) {
        return Err(LocationError::msg(
            "Main function must return unit or an integer.",
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
    let return_type = &function.function.return_type;
    let expected_type = if !always_returns_expr(&function.function.expr) {
        Some(return_type)
    } else {
        None
    };
    let analyzed_expr = analyze_expr(context, &function.function.expr, expected_type)?;

    let stack_space = calc_local_var_stack_space_expr(&function.function.expr);
    Ok(AnalyzedFunction {
        name: function.function.name.clone(),
        args: function.function.args.clone(),
        return_type: return_type.clone(),
        local_var_stack_size: stack_space,
        expr: analyzed_expr,
    })
}

fn analyze_statement(
    context: &mut ValidationContext,
    statement: &SrcStatement,
) -> ValidationResult<AnalyzedStatement> {
    match &statement.statement {
        Statement::Return(expr) => {
            let function = context
                .function_declarations
                .get(context.current_function.as_ref().unwrap())
                .unwrap();
            let return_type = function.return_type.clone();
            if return_type != Type::Unit {
                let expr = expr.as_ref().ok_or(LocationError::msg(
                    &format!(
                        "Return statement needs to return a value of type '{:?}'.",
                        &function.return_type
                    ),
                    &statement.location,
                ))?;
                let analyzed_expr = analyze_expr(context, expr, Some(&return_type))?;
                Ok(AnalyzedStatement::Return(Some(analyzed_expr)))
            } else {
                if let Some(src_expr) = expr {
                    return Err(LocationError::msg(
                        "Return statement cannot return a value.",
                        &src_expr.location,
                    ));
                }
                Ok(AnalyzedStatement::Return(None))
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
            let analyzed_expr = analyze_expr(context, value, Some(var_type))?;
            context.variable_types.insert(name.clone(), var_type.clone());
            Ok(AnalyzedStatement::Declaration {
                var_type: var_type.clone(),
                name: name.clone(),
                value: analyzed_expr,
            })
        }
        Statement::Expr(expr) => {
            let analyzed_expr = analyze_expr(context, expr, None)?;
            Ok(AnalyzedStatement::Expr(analyzed_expr))
        }
        Statement::If {
            condition,
            true_expr,
            false_statement,
        } => {
            let analyzed_condition = analyze_expr(context, condition, Some(&Type::Bool))?;
            let analyzed_true_expr = analyze_expr(context, true_expr, None)?;
            let analyzed_false_statement = if let Some(false_statement) = false_statement {
                Some(Box::new(analyze_statement(context, false_statement)?))
            } else {
                None
            };
            Ok(AnalyzedStatement::If {
                condition: analyzed_condition,
                true_expr: analyzed_true_expr,
                false_statement: analyzed_false_statement,
            })
        }
        Statement::For {
            init,
            condition,
            update,
            body,
        } => {
            let analyzed_init = analyze_statement(context, init)?;
            let analyzed_condition = analyze_expr(context, condition, Some(&Type::Bool))?;
            let analyzed_update = analyze_expr(context, update, None)?;
            let analyzed_body = analyze_expr(context, body, None)?;
            Ok(AnalyzedStatement::For {
                init: Box::new(analyzed_init),
                condition: analyzed_condition,
                update: analyzed_update,
                body: analyzed_body,
            })
        }
        Statement::While {
            condition,
            body,
            is_do_while: _,
        } => {
            let analyzed_condition = analyze_expr(context, condition, Some(&Type::Bool))?;
            let analyzed_body = analyze_expr(context, body, None)?;
            Ok(AnalyzedStatement::While {
                condition: analyzed_condition,
                body: analyzed_body,
                is_do_while: false,
            })
        }
    }
}

fn analyze_expr(
    context: &mut ValidationContext,
    expr: &SrcExpression,
    expected_type: Option<&Type>,
) -> ValidationResult<TypedAnalyzedExpression> {
    match &expr.expr {
        Expression::Block(statements, return_expr) => {
            let mut always_returns = false;
            let analyzed_statements = statements.into_iter().map(|statement| {
                if always_returns {
                    Err(LocationError::msg("Unreachable code.", &statement.location))
                } else {
                    if always_returns_statement(statement) {
                        always_returns = true;
                    }
                    analyze_statement(context, statement)
                }
            }).collect::<ValidationResult<Vec<AnalyzedStatement>>>()?;
            if let Some(return_expr) = return_expr {
                if always_returns {
                    return Err(LocationError::msg(
                        "Unreachable code.",
                        &return_expr.location,
                    ));
                }
                let analyzed_final_expr = analyze_expr(context, return_expr, expected_type)?;
                let final_expr_type = analyzed_final_expr.expr_type.clone();
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Block(analyzed_statements, Some(Box::new(analyzed_final_expr))),
                    final_expr_type,
                ))
            } else {
                if expected_type.is_none_or(|x| *x == Type::Unit) {
                    Ok(TypedAnalyzedExpression::new(
                        AnalyzedExpression::Block(analyzed_statements, None),
                        Type::Unit,
                    ))
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
            let literal_type = literal.get_type();
            if expected_type.is_none_or(|x| *x == literal_type) {
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Literal(literal.clone()),
                    literal_type,
                ))
            } else if let Literal::Integer(int) = literal {
                let expected = expected_type.unwrap().clone();
                let expr_type = match expected {
                    Type::Integer { size: 1 } if *int >= i8::MIN as i64 && *int <= i8::MAX as i64 => {
                        Ok(expected)
                    }
                    Type::Integer { size: 2 } if *int >= i16::MIN as i64 && *int <= i16::MAX as i64 => {
                        Ok(expected)
                    }
                    Type::Integer { size: 4 } if *int >= i32::MIN as i64 && *int <= i32::MAX as i64 => {
                        Ok(expected)
                    }
                    Type::Integer { size: _ } => Err(LocationError::msg(
                        &format!(
                            "Integer literal out of range for type '{:?}'.",
                            expected
                        ),
                        &expr.location,
                    )),
                    _ => Err(LocationError::msg(
                        &format!(
                            "Expected type '{:?}', found type '{:?}'.",
                            expected_type.unwrap(),
                            literal.get_type()
                        ),
                        &expr.location,
                    )),
                }?;
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Literal(literal.clone()),
                    expr_type,
                ))
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
                    let analyzed_expr = analyze_expr(context, inner_expr, expected_type)?;
                    let expr_type = analyzed_expr.expr_type.clone();
                    if analyzed_expr.expr_type.is_integer() {
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Unary {
                                op: op.clone(),
                                expr: Box::new(analyzed_expr),
                            },
                            expr_type,
                        ))
                    } else {
                        Err(LocationError::msg(
                            &format!(
                                "Unary operation requires an integer type. Found type '{:?}'.",
                                &analyzed_expr
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
                    let analyzed_expr = analyze_expr(context, inner_expr, Some(&Type::Bool))?;
                    Ok(TypedAnalyzedExpression::new(
                        AnalyzedExpression::Unary {
                            op: op.clone(),
                            expr: Box::new(analyzed_expr),
                        },
                        Type::Bool,
                    ))
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
            UnaryOp::Increment | UnaryOp::Decrement => {
                if expected_type.is_none_or(|x| x.is_integer()) {
                    if !is_assignable(&inner_expr.expr) {
                        return Err(LocationError::msg(
                            "Expected assignable expression",
                            &inner_expr.location,
                        ));
                    }
                    let analyzed_expr = analyze_expr(context, inner_expr, expected_type)?;
                    let expr_type = analyzed_expr.expr_type.clone();
                    if expr_type.is_integer() {
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Unary {
                                op: op.clone(),
                                expr: Box::new(analyzed_expr),
                            },
                            expr_type,
                        ))
                    } else {
                        Err(LocationError::msg(
                            &format!(
                                "Unary operation requires an integer type. Found type '{:?}'.",
                                &analyzed_expr
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
            UnaryOp::Borrow => {
                if expected_type.is_none_or(|x| x.is_pointer()) {
                    let inner_expected = expected_type.map(|x| if let Type::Pointer(inner) = x { inner.as_ref() } else { unreachable!() });
                    let analyzed_expr = analyze_expr(context, inner_expr, inner_expected)?;
                    let expr_type = analyzed_expr.expr_type.clone();
                    if is_borrowable(&inner_expr.expr) {
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Unary {
                                op: op.clone(),
                                expr: Box::new(analyzed_expr),
                            },
                            Type::Pointer(Box::new(expr_type)),
                        ))
                    } else {
                        Err(LocationError::msg(
                            "Expected borrowable expression",
                            &inner_expr.location,
                        ))
                    }
                } else {
                    Err(LocationError::msg(
                        &format!(
                            "Unary operation yields a pointer type. Found type '{:?}'.",
                            expected_type
                        ),
                        &expr.location,
                    ))
                }
            }
            UnaryOp::Deref => {
                let inner_expected = expected_type.map(|x| Type::Pointer(Box::new(x.clone())));
                let analyzed_expr = analyze_expr(context, inner_expr, inner_expected.as_ref())?;
                let expr_type = analyzed_expr.expr_type.clone();
                if let Type::Pointer(inner) = expr_type {
                    Ok(TypedAnalyzedExpression::new(
                        AnalyzedExpression::Unary {
                            op: op.clone(),
                            expr: Box::new(analyzed_expr),
                        },
                        *inner,
                    ))
                } else {
                    Err(LocationError::msg(
                        "Expected pointer type",
                        &inner_expr.location,
                    ))
                }
            }
        },
        Expression::Binary { op, left, right } => {
            match op {
                BinaryOp::Logical(_) => {
                    if expected_type.is_none_or(|x| *x == Type::Bool) {
                        let analyzed_left = analyze_expr(context, left, Some(&Type::Bool))?;
                        let analyzed_right = analyze_expr(context, right, Some(&Type::Bool))?;
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Binary {
                                op: op.clone(),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                            Type::Bool,
                        ))
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
                BinaryOp::Math(_) => {
                    if expected_type.is_none_or(|x| x.is_integer()) {
                        let analyzed_left = analyze_expr(context, left, expected_type)?;
                        let left_type = analyzed_left.expr_type.clone();
                        if !left_type.is_integer() {
                            return Err(LocationError::msg(
                                &format!(
                                    "Binary operation requires an integer type. Found type '{:?}'.",
                                    &analyzed_left
                                ),
                                &left.location,
                            ));
                        }
                        let analyzed_right = analyze_expr(context, right, Some(&left_type))?;
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Binary {
                                op: op.clone(),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                            left_type,
                        ))
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
                BinaryOp::Comparison(op) => {
                    if expected_type.is_none_or(|x| *x == Type::Bool) {
                        let analyzed_left = analyze_expr(context, left, None)?;
                        let left_type = analyzed_left.expr_type.clone();
                        if *op != BinaryComparisonOp::Equals && *op != BinaryComparisonOp::NotEquals && !left_type.is_integer() {
                            return Err(LocationError::msg(&format!("Comparison operation requires an integer type. Found type '{:?}'.", &left_type), &left.location));
                        }
                        let analyzed_right = analyze_expr(context, right, Some(&left_type))?;
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Binary {
                                op: BinaryOp::Comparison(op.clone()),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                            Type::Bool,
                        ))
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
                BinaryOp::Assign => {
                    if is_assignable(&left.expr) {
                        let analyzed_left = analyze_expr(context, left, expected_type)?;
                        let left_type = analyzed_left.expr_type.clone();
                        let analyzed_right = analyze_expr(context, right, Some(&left_type))?;
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Binary {
                                op: BinaryOp::Assign,
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                            left_type,
                        ))
                    } else {
                        Err(LocationError::msg(
                            "Expected assignable expression",
                            &expr.location,
                        ))
                    }
                }
                BinaryOp::LogicAssign(_) => {
                    if is_assignable(&left.expr) {
                        if expected_type.is_some_and(|x| *x != Type::Bool) {
                            return Err(LocationError::msg(
                                &format!(
                                    "Binary assignment operation yields a boolean type. Found type '{:?}'.",
                                    expected_type
                                ),
                                &expr.location,
                            ));
                        }
                        let analyzed_left = analyze_expr(context, left, Some(&Type::Bool))?;
                        let analyzed_right = analyze_expr(context, right, Some(&Type::Bool))?;
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Binary {
                                op: op.clone(),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                            Type::Bool,
                        ))
                    } else {
                        Err(LocationError::msg(
                            "Expected assignable expression",
                            &expr.location,
                        ))
                    }
                }
                BinaryOp::MathAssign(_) => {
                    if is_assignable(&left.expr) {
                        let analyzed_left = analyze_expr(context, left, expected_type)?;
                        let left_type = analyzed_left.expr_type.clone();
                        if !left_type.is_integer() {
                            return Err(LocationError::msg(&format!("Binary assignment operation requires an integer type. Found type '{:?}'.", &left_type), &left.location));
                        }
                        let analyzed_right = analyze_expr(context, right, Some(&left_type))?;
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Binary {
                                op: op.clone(),
                                left: Box::new(analyzed_left),
                                right: Box::new(analyzed_right),
                            },
                            left_type,
                        ))
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
                let analyzed_condition = analyze_expr(context, condition, Some(&Type::Bool))?;
                let analyzed_true = analyze_expr(context, true_expr, None)?;
                let true_type = analyzed_true.expr_type.clone();
                let analyzed_false = analyze_expr(context, false_expr, Some(&true_type))?;
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Ternary {
                        condition: Box::new(analyzed_condition),
                        true_expr: Box::new(analyzed_true),
                        false_expr: Box::new(analyzed_false),
                    },
                    true_type,
                ))
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
            if expected_type.is_some_and(|x| *x != var_type) {
                return Err(LocationError::msg(
                    &format!(
                        "Expected type '{:?}', but variable is of type '{:?}'.",
                        expected_type.unwrap(), var_type
                    ),
                    &expr.location,
                ));
            }
            Ok(TypedAnalyzedExpression::new(
                AnalyzedExpression::Variable(name.clone()),
                var_type,
            ))
        }
        Expression::FunctionCall { function, args } => {
            let function_decl = context
                .function_declarations
                .get(function)
                .ok_or(LocationError::msg(
                    &format!("Function '{}' has not been declared.", function),
                    &expr.location,
                ))?
                .clone();
            if function_decl.args.len() != args.len() {
                return Err(LocationError::msg(
                    &format!(
                        "Expected {} arguments, found {}",
                        function_decl.args.len(),
                        args.len()
                    ),
                    &expr.location,
                ));
            }
            let return_type = function_decl.return_type.clone();
            if expected_type.is_some_and(|x| *x != return_type) {
                return Err(LocationError::msg(
                    &format!(
                        "Expected type '{:?}', found type '{:?}'.",
                        expected_type, return_type
                    ),
                    &expr.location,
                ));
            }
            let analyzed_exprs = args.iter().zip(function_decl.args.iter()).map(|(arg, expected_arg_type)| {
                analyze_expr(context, arg, Some(expected_arg_type))
            }).collect::<ValidationResult<Vec<TypedAnalyzedExpression>>>();
            Ok(TypedAnalyzedExpression::new(
                AnalyzedExpression::FunctionCall {
                    function: function.clone(),
                    args: analyzed_exprs?,
                },
                return_type,
            ))
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
            let analyzed_expr = analyze_expr(context, expr, None)?;
            let expr_type = analyzed_expr.expr_type.clone();
            if expr_type.can_cast_to(var_type) {
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Cast {
                        var_type: var_type.clone(),
                        expr: Box::new(analyzed_expr),
                    },
                    var_type.clone(),
                ))
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
        Expression::Unary { op: UnaryOp::Deref, expr } => is_assignable(&expr.expr),
        _ => false,
    }
}

fn is_borrowable(expr: &Expression) -> bool {
    match expr {
        Expression::Variable(_) => true,
        Expression::Unary { op: UnaryOp::Deref, expr } => is_borrowable(&expr.expr),
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
            BinaryOp::Logical(_) => always_returns_expr(left),
            BinaryOp::LogicAssign(_) => always_returns_expr(left),
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
            false_statement,
            ..
        } => {
            always_returns_expr(true_expr)
                && false_statement
                .as_ref()
                .map(|x| always_returns_statement(x))
                .unwrap_or(false)
        }
        Statement::For {
            init,
            condition,
            update: _,
            body: _,
        } => {
            always_returns_statement(init)
                || always_returns_expr(condition)
        }
        Statement::While { condition, body, is_do_while } => {
            always_returns_expr(condition) || (*is_do_while && always_returns_expr(body))
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
            false_statement,
        } => {
            calc_local_var_stack_space_expr(condition)
                + calc_local_var_stack_space_expr(true_expr)
                + false_statement
                .as_ref()
                .map(|x| calc_local_var_stack_space_statement(x))
                .unwrap_or(0)
        }
        Statement::For {
            init,
            condition,
            update,
            body,
        } => {
            calc_local_var_stack_space_statement(init)
                + calc_local_var_stack_space_expr(condition)
                + calc_local_var_stack_space_expr(update)
                + calc_local_var_stack_space_expr(body)
        }
        Statement::While { condition, body, .. } => {
            calc_local_var_stack_space_expr(condition) + calc_local_var_stack_space_expr(body)
        }
    }
}
