use std::collections::HashMap;
use crate::compiler::parser::syntax_tree::{BinaryOp, Literal, UnaryOp};
use crate::compiler::parser::types::Type;

#[derive(Debug, Clone)]
pub struct AnalyzedProgram {
    pub functions: HashMap<String, AnalyzedFunction>,
    pub main_function: String,
}

#[derive(Debug, Clone)]
pub struct AnalyzedFunction {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
    pub local_var_stack_size: usize,
    pub expr: TypedAnalyzedExpression,
}

#[derive(Debug, Clone)]
pub struct TypedAnalyzedExpression {
    pub expr: AnalyzedExpression,
    pub expr_type: Type,
}

impl TypedAnalyzedExpression {
    pub fn new(expr: AnalyzedExpression, expr_type: Type) -> Self {
        Self { expr, expr_type }
    }
}

#[derive(Debug, Clone)]
pub enum AnalyzedStatement {
    Return(Option<TypedAnalyzedExpression>),
    Declaration {
        var_type: Type,
        name: String,
        value: TypedAnalyzedExpression,
    },
    If {
        condition: TypedAnalyzedExpression,
        true_expr: TypedAnalyzedExpression,
        false_statement: Option<Box<AnalyzedStatement>>,
    },
    For {
        init: Box<AnalyzedStatement>,
        condition: TypedAnalyzedExpression,
        update: TypedAnalyzedExpression,
        body: TypedAnalyzedExpression,
    },
    While {
        condition: TypedAnalyzedExpression,
        body: TypedAnalyzedExpression,
        is_do_while: bool,
    },
    Expr(TypedAnalyzedExpression),
}

#[derive(Debug, Clone)]
pub enum AnalyzedExpression {
    Block(Vec<AnalyzedStatement>, Option<Box<TypedAnalyzedExpression>>),
    Binary {
        op: BinaryOp,
        left: Box<TypedAnalyzedExpression>,
        right: Box<TypedAnalyzedExpression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<TypedAnalyzedExpression>,
    },
    Literal(Literal),
    Variable(String),
    Ternary {
        condition: Box<TypedAnalyzedExpression>,
        true_expr: Box<TypedAnalyzedExpression>,
        false_expr: Box<TypedAnalyzedExpression>,
    },
    FunctionCall {
        function: String,
        args: Vec<TypedAnalyzedExpression>,
    },
    Cast {
        var_type: Type,
        expr: Box<TypedAnalyzedExpression>,
    },
}