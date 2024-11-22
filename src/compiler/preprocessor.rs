use crate::compiler::parser::syntax_tree::{Expression, Literal, Program, SrcExpression, SrcStatement, Statement, UnaryOp};

pub fn preprocess(mut program: Program) -> Program {
    merge_negative_literals(&mut program);
    program
}

fn merge_negative_literals(program: &mut Program) {
    for func in &mut program.functions {
        merge_negative_literals_expr(&mut func.function.expr);
    }
}

fn merge_negative_literals_expr(expr: &mut SrcExpression) {
    let mut replace_expr = None;
    match &mut expr.expr {
        Expression::Unary { op, expr: inner_expr } => {
            if *op == UnaryOp::Negate {
                if let Expression::Literal(Literal::Integer(n)) = &inner_expr.expr {
                    replace_expr = Some(Expression::Literal(Literal::Integer(-n)));
                }
            }
            merge_negative_literals_expr(inner_expr);
        }
        Expression::Literal(_) => {}
        Expression::Variable(_) => {}
        Expression::Binary { left, right, op: _ } => {
            merge_negative_literals_expr(left);
            merge_negative_literals_expr(right);
        }
        Expression::Ternary { condition, true_expr, false_expr } => {
            merge_negative_literals_expr(condition);
            merge_negative_literals_expr(true_expr);
            merge_negative_literals_expr(false_expr);
        }
        Expression::FunctionCall { function: _, args } => {
            for arg in args {
                merge_negative_literals_expr(arg);
            }
        }
        Expression::Cast { var_type: _, expr } => {
            merge_negative_literals_expr(expr);
        }
        Expression::Block(statements, last_expr) => {
            for statement in statements {
                merge_negative_literals_statement(statement);
            }
            if let Some(last_expr) = last_expr {
                merge_negative_literals_expr(last_expr);
            }
        }
    }
    if let Some(replace_expr) = replace_expr {
        expr.expr = replace_expr;
    }
}

fn merge_negative_literals_statement(statement: &mut SrcStatement) {
    match &mut statement.statement {
        Statement::Expr(expr) => merge_negative_literals_expr(expr),
        Statement::If { condition, true_expr, false_statement } => {
            merge_negative_literals_expr(condition);
            merge_negative_literals_expr(true_expr);
            if let Some(else_body) = false_statement {
                merge_negative_literals_statement(else_body);
            }
        }
        Statement::While { condition, body, is_do_while: _ } => {
            merge_negative_literals_expr(condition);
            merge_negative_literals_expr(body);
        }
        Statement::For { init, condition, update, body } => {
            merge_negative_literals_statement(init);
            merge_negative_literals_expr(condition);
            merge_negative_literals_expr(update);
            merge_negative_literals_expr(body);
        }
        Statement::Return(expr) => {
            if let Some(expr) = expr {
                merge_negative_literals_expr(expr);
            }
        }
        Statement::Declaration { var_type: _, name: _, value } => {
            merge_negative_literals_expr(value);
        }
    }
}