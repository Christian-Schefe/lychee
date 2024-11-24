use std::collections::HashMap;
use crate::compiler::parser::syntax_tree::{BinaryOp, Literal, UnaryOp};
use crate::compiler::parser::types::Type;

#[derive(Debug, Clone)]
pub struct AnalyzedProgram {
    pub functions: HashMap<String, AnalyzedFunction>,
    pub main_function: String,
    pub struct_definitions: HashMap<String, AnalyzedStructDefinition>,
}

#[derive(Debug, Clone)]
pub struct AnalyzedStructDefinition {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct AnalyzedFunction {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
    pub local_var_stack_size: usize,
    pub expr: TypedAnalyzedExpression,
}

pub type TypedAnalyzedExpression = Typed<AnalyzedExpression>;
pub type TypedAnalyzedAddressableExpression = Typed<AnalyzedAddressableExpression>;

#[derive(Debug, Clone)]
pub struct Typed<T> {
    pub value: T,
    pub ty: Type,
}

impl<T> Typed<T> {
    pub fn new(value: T, ty: Type) -> Self {
        Self { value, ty }
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
    BinaryAssign {
        op: BinaryOp,
        left: Box<TypedAnalyzedAddressableExpression>,
        right: Box<TypedAnalyzedExpression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<TypedAnalyzedExpression>,
    },
    Sizeof(Type),
    Literal(Literal),
    Ternary {
        condition: Box<TypedAnalyzedExpression>,
        true_expr: Box<TypedAnalyzedExpression>,
        false_expr: Box<TypedAnalyzedExpression>,
    },
    FunctionCall {
        function: String,
        args: HashMap<String, TypedAnalyzedExpression>,
    },
    Cast {
        var_type: Type,
        expr: Box<TypedAnalyzedExpression>,
    },
    StructLiteral {
        name: String,
        fields: HashMap<String, TypedAnalyzedExpression>,
    },
    Increment {
        expr: Box<TypedAnalyzedAddressableExpression>,
        is_increment: bool,
        postfix: bool,
    },
    Addressable(TypedAnalyzedAddressableExpression),
    Borrow(TypedAnalyzedAddressableExpression),
}

#[derive(Debug, Clone)]
pub enum AnalyzedAddressableExpression {
    Variable(String),
    MemberAccess {
        expr: Box<TypedAnalyzedAddressableExpression>,
        member: String,
        struct_name: String,
    },
    Dereference(Box<TypedAnalyzedExpression>),
}
