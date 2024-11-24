use crate::compiler::parser::syntax_tree::{Literal, SrcStructDefinition};
use crate::compiler::lexer::Location;
use crate::compiler::parser::analyzed_syntax_tree::{AnalyzedAddressableExpression, AnalyzedExpression, AnalyzedFunction, AnalyzedProgram, AnalyzedStatement, AnalyzedStructDefinition, TypedAnalyzedAddressableExpression, TypedAnalyzedExpression};
use crate::compiler::parser::parser_error::LocationError;
use crate::compiler::parser::syntax_tree::{BinaryOp, BinaryComparisonOp, Expression, Program, SrcExpression, SrcFunction, SrcStatement, Statement, UnaryOp};
use crate::compiler::parser::types::Type;
use std::collections::{HashMap, HashSet};
use anyhow::Error;
use crate::compiler::parser::type_resolver::{build_resolved_types, resolve_type, ResolvedTypes};

pub type ValidationResult<T> = Result<T, Error>;

struct ValidationContext {
    function_declarations: HashMap<String, FunctionDeclaration>,
    current_function: Option<String>,
    variable_types: HashMap<String, Type>,
    resolved_types: ResolvedTypes,
}

#[derive(Clone)]
struct FunctionDeclaration {
    return_type: Type,
    args: Vec<(String, Type)>,
}

fn add_builtin_function_declarations(context: &mut ValidationContext) {
    let write_declaration = FunctionDeclaration {
        return_type: Type::Unit,
        args: vec![("char".to_string(), Type::Char)],
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
        resolved_types: build_resolved_types(program)?,
    };
    add_builtin_function_declarations(&mut context);

    let main_function = program
        .functions
        .iter()
        .find(|f| f.value.name == "main")
        .ok_or(LocationError::msg(
            "Program requires a main function.",
            &Location { line: 1, column: 1 },
        ))?;
    let main_return_type = resolve_type(&context.resolved_types, &main_function.value.return_type)?;
    if main_return_type != Type::Unit && !matches!(main_return_type, Type::Integer { size: _ }) {
        return Err(LocationError::msg(
            "Main function must return unit or an integer.",
            &main_function.location,
        ));
    }

    for function in &program.functions {
        let mut resolved_args = Vec::with_capacity(function.value.args.len());
        let mut used_names = HashSet::new();
        for (name, ty) in &function.value.args {
            let ty = resolve_type(&context.resolved_types, ty)?;
            resolved_args.push((name.clone(), ty));
            if !used_names.insert(name) {
                return Err(LocationError::msg(
                    &format!("Argument '{}' has already been declared.", name),
                    &function.location,
                ));
            }
        }
        let declaration = FunctionDeclaration {
            return_type: resolve_type(&context.resolved_types, &function.value.return_type)?,
            args: resolved_args,
        };
        if context.function_declarations.contains_key(&function.value.name)
        {
            return Err(LocationError::msg(
                &format!(
                    "Function '{}' has already been declared.",
                    function.value.name
                ),
                &function.location,
            ));
        }
        context.function_declarations.insert(function.value.name.clone(), declaration);
    }
    let mut analyzed_functions = HashMap::new();
    for function in &program.functions {
        let analyzed_fn = analyze_function(&mut context, function)?;
        analyzed_functions.insert(function.value.name.clone(), analyzed_fn);
    }
    let mut analyzed_structs = HashMap::new();
    for struct_def in &program.struct_definitions {
        let analyzed_struct = analyze_struct_definition(&context, struct_def)?;
        analyzed_structs.insert(struct_def.value.name.clone(), analyzed_struct);
    }
    Ok(AnalyzedProgram {
        functions: analyzed_functions,
        main_function: main_function.value.name.clone(),
        struct_definitions: analyzed_structs,
    })
}

fn analyze_struct_definition(
    context: &ValidationContext,
    struct_def: &SrcStructDefinition,
) -> ValidationResult<AnalyzedStructDefinition> {
    let mut resolved_fields = Vec::with_capacity(struct_def.value.fields.len());
    let mut used_names = HashSet::new();
    for (name, ty) in &struct_def.value.fields {
        let resolved_ty = resolve_type(&context.resolved_types, ty)?;
        resolved_fields.push((name.clone(), resolved_ty));
        if !used_names.insert(name) {
            return Err(LocationError::msg(
                &format!("Field '{}' has already been declared.", name),
                &struct_def.location,
            ));
        }
    }
    Ok(AnalyzedStructDefinition {
        name: struct_def.value.name.clone(),
        fields: resolved_fields,
    })
}

fn analyze_function(
    context: &mut ValidationContext,
    function: &SrcFunction,
) -> ValidationResult<AnalyzedFunction> {
    context.current_function = Some(function.value.name.clone());
    context.variable_types = HashMap::new();
    for (name, ty) in &function.value.args {
        let ty = resolve_type(&context.resolved_types, ty)?;
        context.variable_types.insert(name.clone(), ty);
    }
    let return_type = resolve_type(&context.resolved_types, &function.value.return_type)?;
    let expected_type = if !always_returns_expr(&function.value.expr) {
        Some(&return_type)
    } else {
        None
    };
    let analyzed_expr = analyze_expr(context, &function.value.expr, expected_type)?;

    let stack_space = calc_local_var_stack_space_expr(&analyzed_expr);
    Ok(AnalyzedFunction {
        name: function.value.name.clone(),
        args: context.function_declarations[&function.value.name].args.clone(),
        return_type: return_type.clone(),
        local_var_stack_size: stack_space,
        expr: analyzed_expr,
    })
}

fn analyze_statement(
    context: &mut ValidationContext,
    statement: &SrcStatement,
) -> ValidationResult<AnalyzedStatement> {
    match &statement.value {
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
            let resolved_var_type = resolve_type(&context.resolved_types, var_type)?;
            let analyzed_expr = analyze_expr(context, value, Some(&resolved_var_type))?;
            context.variable_types.insert(name.clone(), resolved_var_type.clone());
            Ok(AnalyzedStatement::Declaration {
                var_type: resolved_var_type,
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
    match &expr.value {
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
                let final_expr_type = analyzed_final_expr.ty.clone();
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
                            "Integer literal out of range for type '{}'.",
                            expected
                        ),
                        &expr.location,
                    )),
                    _ => Err(LocationError::msg(
                        &format!(
                            "Expected type '{}', found type '{}'.",
                            expected,
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
                    let expr_type = analyzed_expr.ty.clone();
                    if analyzed_expr.ty.is_integer() {
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
                        let left_type = analyzed_left.ty.clone();
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
                        let left_type = analyzed_left.ty.clone();
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
                    let analyzed_left = analyze_expr(context, left, expected_type)?;
                    match analyzed_left.value {
                        AnalyzedExpression::Addressable(adr) => {
                            let left_type = adr.ty.clone();
                            let analyzed_right = analyze_expr(context, right, Some(&left_type))?;
                            Ok(TypedAnalyzedExpression::new(
                                AnalyzedExpression::BinaryAssign {
                                    op: BinaryOp::Assign,
                                    left: Box::new(adr),
                                    right: Box::new(analyzed_right),
                                },
                                left_type,
                            ))
                        }
                        _ => Err(LocationError::msg(
                            "Expected addressable expression",
                            &left.location,
                        )),
                    }
                }
                BinaryOp::LogicAssign(_) => {
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
                    match analyzed_left.value {
                        AnalyzedExpression::Addressable(adr) => {
                            let analyzed_right = analyze_expr(context, right, Some(&Type::Bool))?;
                            Ok(TypedAnalyzedExpression::new(
                                AnalyzedExpression::BinaryAssign {
                                    op: op.clone(),
                                    left: Box::new(adr),
                                    right: Box::new(analyzed_right),
                                },
                                Type::Bool,
                            ))
                        }
                        _ => Err(LocationError::msg(
                            "Expected addressable expression",
                            &left.location,
                        )),
                    }
                }
                BinaryOp::MathAssign(_) => {
                    let analyzed_left = analyze_expr(context, left, expected_type)?;
                    match analyzed_left.value {
                        AnalyzedExpression::Addressable(adr) => {
                            let left_type = adr.ty.clone();
                            if !left_type.is_integer() {
                                return Err(LocationError::msg(&format!("Binary assignment operation requires an integer type. Found type '{:?}'.", &left_type), &left.location));
                            }
                            let analyzed_right = analyze_expr(context, right, Some(&left_type))?;
                            Ok(TypedAnalyzedExpression::new(
                                AnalyzedExpression::BinaryAssign {
                                    op: op.clone(),
                                    left: Box::new(adr),
                                    right: Box::new(analyzed_right),
                                },
                                left_type,
                            ))
                        }
                        _ => Err(LocationError::msg(
                            "Expected addressable expression",
                            &left.location,
                        )),
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
                let true_type = analyzed_true.ty.clone();
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
                AnalyzedExpression::Addressable(TypedAnalyzedAddressableExpression::new(
                    AnalyzedAddressableExpression::Variable(name.clone()),
                    var_type.clone(),
                )),
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
            let mut analyzed_exprs = HashMap::with_capacity(args.len());
            for (arg, (arg_name, arg_type)) in args.iter().zip(function_decl.args.iter()) {
                let analyzed_expr = analyze_expr(context, arg, Some(arg_type))?;
                analyzed_exprs.insert(arg_name.clone(), analyzed_expr);
            }
            Ok(TypedAnalyzedExpression::new(
                AnalyzedExpression::FunctionCall {
                    function: function.clone(),
                    args: analyzed_exprs,
                },
                return_type,
            ))
        }
        Expression::Cast { var_type, expr } => {
            let var_type = resolve_type(&context.resolved_types, var_type)?;
            if let Some(expected_type) = expected_type {
                if var_type != *expected_type {
                    return Err(LocationError::msg(
                        &format!(
                            "Expected type '{:?}', found cast to type '{:?}'.",
                            expected_type, var_type
                        ),
                        &expr.location,
                    ));
                }
            }
            let analyzed_expr = analyze_expr(context, expr, None)?;
            let expr_type = analyzed_expr.ty.clone();
            if expr_type.can_cast_to(&var_type) {
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Cast {
                        var_type: var_type.clone(),
                        expr: Box::new(analyzed_expr),
                    },
                    var_type,
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
        Expression::Sizeof(ty) => {
            let ty = resolve_type(&context.resolved_types, ty)?;
            if expected_type.is_none_or(|x| *x == Type::Integer { size: 4 }) {
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Sizeof(ty),
                    Type::Integer { size: 4 },
                ))
            } else {
                Err(LocationError::msg(
                    &format!(
                        "Sizeof operation yields an integer type. Found type '{:?}'.",
                        expected_type
                    ),
                    &expr.location,
                ))
            }
        }
        Expression::StructLiteral { struct_type, fields } => {
            let struct_type = resolve_type(&context.resolved_types, struct_type)?;
            let (name, struct_fields) = match &struct_type {
                Type::Struct { name, fields } => (name, fields),
                _ => unreachable!("Struct type must be resolved to a struct type."),
            };
            if struct_fields.len() != fields.len() {
                return Err(LocationError::msg(
                    &format!(
                        "Expected {} fields, found {}",
                        struct_fields.len(),
                        fields.len()
                    ),
                    &expr.location,
                ));
            }
            let analyzed_fields = fields.iter().map(|(field_name, field_expr)| {
                let field_type = struct_fields
                    .get(field_name)
                    .cloned()
                    .ok_or(LocationError::msg(
                        &format!(
                            "Field '{}' has not been declared in struct '{}'.",
                            field_name, name
                        ),
                        &expr.location,
                    ))?;
                let analyzed_expr = analyze_expr(context, field_expr, Some(&field_type))?;
                Ok((field_name.clone(), analyzed_expr))
            }).collect::<ValidationResult<HashMap<String, TypedAnalyzedExpression>>>()?;
            Ok(TypedAnalyzedExpression::new(
                AnalyzedExpression::StructLiteral {
                    name: name.clone(),
                    fields: analyzed_fields,
                },
                struct_type,
            ))
        }
        Expression::Increment { expr, is_increment, postfix } => {
            if expected_type.is_none_or(|x| x.is_integer()) {
                let analyzed_expr = analyze_expr(context, expr, expected_type)?;
                let expr_type = analyzed_expr.ty.clone();
                if !expr_type.is_integer() {
                    return Err(LocationError::msg(
                        &format!(
                            "Increment operation requires an integer type. Found type '{:?}'.",
                            &analyzed_expr
                        ),
                        &expr.location,
                    ));
                }
                match analyzed_expr.value {
                    AnalyzedExpression::Addressable(adr) => Ok(TypedAnalyzedExpression::new(
                        AnalyzedExpression::Increment {
                            is_increment: *is_increment,
                            expr: Box::new(adr),
                            postfix: *postfix,
                        },
                        expr_type,
                    )),
                    _ => Err(LocationError::msg(
                        "Expected addressable expression",
                        &expr.location,
                    )),
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
        Expression::MemberAccess { expr, member } => {
            let analyzed_expr = analyze_expr(context, expr, None)?;
            let expr_type = analyzed_expr.ty.clone();
            let (struct_name, member_type) = match &expr_type {
                Type::Struct { fields, name } => (name.clone(), fields
                    .get(member)
                    .cloned()
                    .ok_or(LocationError::msg(
                        &format!(
                            "Field '{}' has not been declared in struct '{}'.",
                            member, expr_type
                        ),
                        &expr.location,
                    ))?),
                _ => {
                    return Err(LocationError::msg(
                        &format!(
                            "Member access requires a struct type. Found type '{:?}'.",
                            expr_type
                        ),
                        &expr.location,
                    ))
                }
            };
            if expected_type.is_none_or(|x| *x == member_type) {
                match analyzed_expr.value {
                    AnalyzedExpression::Addressable(adr) => {
                        Ok(TypedAnalyzedExpression::new(
                            AnalyzedExpression::Addressable(TypedAnalyzedAddressableExpression::new(AnalyzedAddressableExpression::MemberAccess {
                                expr: Box::new(adr),
                                member: member.clone(),
                                struct_name,
                            }, member_type.clone())),
                            member_type,
                        ))
                    }
                    _ => Err(LocationError::msg(
                        "Expected addressable expression",
                        &expr.location,
                    )),
                }
            } else {
                Err(LocationError::msg(
                    &format!(
                        "Expected type '{:?}', found type '{:?}'.",
                        expected_type, member_type
                    ),
                    &expr.location,
                ))
            }
        }
        Expression::Borrow(inner_expr) => {
            if expected_type.is_none_or(|x| x.is_pointer()) {
                let inner_expected = expected_type.map(|x| if let Type::Pointer(inner) = x { inner.as_ref() } else { unreachable!() });
                let analyzed_expr = analyze_expr(context, inner_expr, inner_expected)?;
                let expr_type = analyzed_expr.ty.clone();
                match analyzed_expr.value {
                    AnalyzedExpression::Addressable(adr) => Ok(TypedAnalyzedExpression::new(
                        AnalyzedExpression::Borrow(adr),
                        Type::Pointer(Box::new(expr_type)),
                    )),
                    _ => Err(LocationError::msg(
                        "Expected addressable expression",
                        &inner_expr.location,
                    )),
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
        Expression::Dereference(inner_expr) => {
            let inner_expected = expected_type.map(|x| Type::Pointer(Box::new(x.clone())));
            let analyzed_expr = analyze_expr(context, inner_expr, inner_expected.as_ref())?;
            let expr_type = analyzed_expr.ty.clone();
            if let Type::Pointer(inner) = expr_type {
                Ok(TypedAnalyzedExpression::new(
                    AnalyzedExpression::Addressable(TypedAnalyzedAddressableExpression::new(AnalyzedAddressableExpression::Dereference(Box::new(analyzed_expr)), *inner.clone())),
                    *inner,
                ))
            } else {
                Err(LocationError::msg(
                    "Expected pointer type",
                    &inner_expr.location,
                ))
            }
        }
    }
}

fn always_returns_expr(expr: &SrcExpression) -> bool {
    match &expr.value {
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
        Expression::Sizeof(_) => false,
        Expression::StructLiteral { struct_type: _, fields } => fields.iter().any(|(_, expr)| always_returns_expr(expr)),
        Expression::Increment { expr, .. } => always_returns_expr(expr),
        Expression::MemberAccess { expr, member: _ } => always_returns_expr(expr),
        Expression::Borrow(expr) => always_returns_expr(expr),
        Expression::Dereference(expr) => always_returns_expr(expr),
    }
}

fn always_returns_statement(statement: &SrcStatement) -> bool {
    match &statement.value {
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

fn calc_local_var_stack_space_expr(expression: &TypedAnalyzedExpression) -> usize {
    match &expression.value {
        AnalyzedExpression::Block(statements, return_expr) => {
            let mut stack_space = 0;
            for statement in statements {
                stack_space += calc_local_var_stack_space_statement(statement);
            }
            if let Some(return_expr) = return_expr {
                stack_space += calc_local_var_stack_space_expr(return_expr);
            }
            stack_space
        }
        AnalyzedExpression::Literal(_) => 0,
        AnalyzedExpression::Unary { op: _, expr } => calc_local_var_stack_space_expr(expr),
        AnalyzedExpression::Binary { op: _, left, right } => {
            calc_local_var_stack_space_expr(left) + calc_local_var_stack_space_expr(right)
        }
        AnalyzedExpression::Ternary {
            condition,
            true_expr,
            false_expr,
        } => {
            calc_local_var_stack_space_expr(condition)
                + calc_local_var_stack_space_expr(true_expr)
                + calc_local_var_stack_space_expr(false_expr)
        }
        AnalyzedExpression::FunctionCall { function: _, args } => {
            args.iter().map(|(_, x)| calc_local_var_stack_space_expr(x)).sum()
        }
        AnalyzedExpression::Cast { var_type: _, expr } => calc_local_var_stack_space_expr(expr),
        AnalyzedExpression::Sizeof(_) => 0,
        AnalyzedExpression::StructLiteral { name: _, fields } => {
            fields.iter().map(|(_, expr)| calc_local_var_stack_space_expr(expr)).sum()
        }
        AnalyzedExpression::BinaryAssign { op: _, left, right } => {
            calc_local_var_stack_space_addressable_expr(left) + calc_local_var_stack_space_expr(right)
        }
        AnalyzedExpression::Increment { expr, .. } => calc_local_var_stack_space_addressable_expr(expr),
        AnalyzedExpression::Addressable(adr) => calc_local_var_stack_space_addressable_expr(adr),
        AnalyzedExpression::Borrow(adr) => calc_local_var_stack_space_addressable_expr(adr),
    }
}

fn calc_local_var_stack_space_addressable_expr(expression: &TypedAnalyzedAddressableExpression) -> usize {
    match &expression.value {
        AnalyzedAddressableExpression::Variable(_) => 0,
        AnalyzedAddressableExpression::MemberAccess { expr, .. } => {
            calc_local_var_stack_space_addressable_expr(expr)
        }
        AnalyzedAddressableExpression::Dereference(expr) => {
            calc_local_var_stack_space_expr(expr)
        }
    }
}

fn calc_local_var_stack_space_statement(statement: &AnalyzedStatement) -> usize {
    match statement {
        AnalyzedStatement::Return(expr) => expr
            .as_ref()
            .map(calc_local_var_stack_space_expr)
            .unwrap_or(0),
        AnalyzedStatement::Declaration { var_type, .. } => var_type.size(),
        AnalyzedStatement::Expr(expr) => calc_local_var_stack_space_expr(expr),
        AnalyzedStatement::If {
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
        AnalyzedStatement::For {
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
        AnalyzedStatement::While { condition, body, .. } => {
            calc_local_var_stack_space_expr(condition) + calc_local_var_stack_space_expr(body)
        }
    }
}
