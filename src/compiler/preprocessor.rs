use crate::compiler::parser::syntax_tree::{Expression, Literal, Program, SrcExpression, SrcStatement, Statement, UnaryOp};

pub fn preprocess(mut program: Program) -> Program {
    preprocess_program(&mut program);
    program
}

fn preprocess_program(program: &mut Program) {
    for func in &mut program.functions {
        preprocess_expression(&mut func.value.expr);
    }
}

fn preprocess_expression(expr: &mut SrcExpression) {
    let mut replace_expr = None;
    match &mut expr.value {
        Expression::Unary { op, expr: inner_expr } => {
            if *op == UnaryOp::Negate {
                if let Expression::Literal(Literal::Integer(n)) = &inner_expr.value {
                    replace_expr = Some(Expression::Literal(Literal::Integer(-n)));
                }
            }
            preprocess_expression(inner_expr);
        }
        Expression::Literal(_) => {}
        Expression::Variable(_) => {}
        Expression::Binary { left, right, op: _ } => {
            preprocess_expression(left);
            preprocess_expression(right);
        }
        Expression::Ternary { condition, true_expr, false_expr } => {
            preprocess_expression(condition);
            preprocess_expression(true_expr);
            preprocess_expression(false_expr);
        }
        Expression::FunctionCall { function: _, args } => {
            for arg in args {
                preprocess_expression(arg);
            }
        }
        Expression::Cast { var_type: _, expr } => {
            preprocess_expression(expr);
        }
        Expression::Block(statements, last_expr) => {
            for statement in statements {
                preprocess_statement(statement);
            }
            if let Some(last_expr) = last_expr {
                preprocess_expression(last_expr);
            }
        }
        Expression::Sizeof(_) => {}
        Expression::StructLiteral { struct_type: _, fields } => {
            for (_, field_expr) in fields {
                preprocess_expression(field_expr);
            }
        }
        Expression::MemberAccess { expr, member: _ } => {
            preprocess_expression(expr);
        }
        Expression::Dereference(inner_expr) => {
            preprocess_expression(inner_expr);
        }
        Expression::Borrow(inner_expr) => {
            preprocess_expression(inner_expr);
        }
        Expression::Increment { expr, is_increment: _, postfix: _ } => {
            preprocess_expression(expr);
        }
    }
    if let Some(replace_expr) = replace_expr {
        expr.value = replace_expr;
    }
}

fn preprocess_statement(statement: &mut SrcStatement) {
    match &mut statement.value {
        Statement::Expr(expr) => preprocess_expression(expr),
        Statement::If { condition, true_expr, false_statement } => {
            preprocess_expression(condition);
            preprocess_expression(true_expr);
            if let Some(else_body) = false_statement {
                preprocess_statement(else_body);
            }
        }
        Statement::While { condition, body, is_do_while: _ } => {
            preprocess_expression(condition);
            preprocess_expression(body);
        }
        Statement::For { init, condition, update, body } => {
            preprocess_statement(init);
            preprocess_expression(condition);
            preprocess_expression(update);
            preprocess_expression(body);
        }
        Statement::Return(expr) => {
            if let Some(expr) = expr {
                preprocess_expression(expr);
            }
        }
        Statement::Declaration { var_type: _, name: _, value } => {
            preprocess_expression(value);
        }
    }
}