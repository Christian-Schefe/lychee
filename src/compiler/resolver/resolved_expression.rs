use crate::compiler::analyzer::analyzed_expression::{AnalyzedBinaryOp, BinaryAssignOp};
use crate::compiler::analyzer::type_resolver::AnalyzedType;
use crate::compiler::parser2::parsed_expression::{UnaryMathOp};

#[derive(Debug, Clone)]
pub struct ResolvedProgram {
    pub functions: Vec<ResolvedFunction>,
}

#[derive(Debug, Clone)]
pub enum ValueLocation {
    Stack,
    Register,
    None,
}

impl ValueLocation {
    pub fn from_type(ty: &AnalyzedType) -> ValueLocation {
        match ty {
            AnalyzedType::Unit => ValueLocation::None,
            AnalyzedType::Struct(_) => ValueLocation::Stack,
            _ => ValueLocation::Register,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedFunction {
    pub name: String,
    pub body: ResolvedExpression,
    pub value_location: ValueLocation,
    pub local_var_stack_size: usize,
}

#[derive(Debug, Clone)]
pub struct ResolvedExpression {
    pub kind: ResolvedExpressionKind,
    pub value_location: ValueLocation,
    pub stack_discard: usize,
}

#[derive(Debug, Clone)]
pub enum ResolvedExpressionKind {
    Block(Vec<ResolvedExpression>),
    Return(Option<Box<ResolvedExpression>>),
    Continue,
    Break {
        maybe_expr: Option<Box<ResolvedExpression>>,
    },
    If {
        condition: Box<ResolvedExpression>,
        then_block: Box<ResolvedExpression>,
        else_expr: Option<Box<ResolvedExpression>>,
    },
    While {
        condition: Box<ResolvedExpression>,
        loop_body: Box<ResolvedExpression>,
        else_expr: Option<Box<ResolvedExpression>>,
    },
    Declaration {
        var_offset: isize,
        value: Box<ResolvedExpression>,
    },
    Variable(isize),
    Literal(ResolvedLiteral),
    Unary {
        op: ResolvedUnaryOp,
        expr: Box<ResolvedExpression>,
    },
    Binary {
        op: AnalyzedBinaryOp,
        left: Box<ResolvedExpression>,
        right: Box<ResolvedExpression>,
    },
    Assign {
        op: BinaryAssignOp,
        lhs: ResolvedAssignableExpression,
        rhs: Box<ResolvedExpression>,
    },
    Borrow {
        expr: ResolvedAssignableExpression,
    },
    FunctionCall {
        function_name: String,
        args: Vec<ResolvedExpression>,
    },
    FieldAccess {
        expr: Box<ResolvedExpression>,
        field_offset: usize,
    },
    ArrayIndex {
        array: Box<ResolvedExpression>,
        index: Box<ResolvedExpression>,
        element_size: usize,
    },
    Increment(ResolvedAssignableExpression, bool),
    Decrement(ResolvedAssignableExpression, bool),
}

#[derive(Debug, Clone)]
pub enum ResolvedLiteral {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
    Struct(Vec<ResolvedExpression>),
    Array(Vec<ResolvedExpression>),
}

#[derive(Debug, Clone)]
pub enum ResolvedAssignableExpression {
    LocalVariable(isize),
    Dereference(Box<ResolvedExpression>),
    FieldAccess(Box<ResolvedAssignableExpression>, usize),
    ArrayIndex(Box<ResolvedAssignableExpression>, Box<ResolvedExpression>, usize),
}

#[derive(Debug, Clone)]
pub enum ResolvedUnaryOp {
    Math(UnaryMathOp),
    LogicalNot,
    Dereference,
    IntCast {
        from: usize,
        to: usize,
    },
    BoolCast,
}
