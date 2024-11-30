use crate::compiler::analyzer::type_resolver::{AnalyzedType, AnalyzedTypes};
use crate::compiler::parser2::parsed_expression::{BinaryComparisonOp, BinaryLogicOp, BinaryMathOp, UnaryMathOp};

#[derive(Debug, Clone)]
pub struct AnalyzedProgram {
    pub analyzed_types: AnalyzedTypes,
    pub functions: Vec<AnalyzedFunction>,
    pub main_function: usize,
}

#[derive(Debug, Clone)]
pub struct AnalyzedFunction {
    pub name: String,
    pub return_type: AnalyzedType,
    pub parameters: Vec<(String, AnalyzedType)>,
    pub body: AnalyzedExpression,
}

#[derive(Debug, Clone)]
pub struct AnalyzedExpression {
    pub kind: AnalyzedExpressionKind,
    pub ty: AnalyzedType,
}


#[derive(Debug, Clone)]
pub enum AnalyzedExpressionKind {
    Block {
        expressions: Vec<AnalyzedExpression>,
        returns_value: bool,
    },
    Return(Option<Box<AnalyzedExpression>>),
    Continue,
    Break(Option<Box<AnalyzedExpression>>),
    If {
        condition: Box<AnalyzedExpression>,
        then_block: Box<AnalyzedExpression>,
        else_expr: Option<Box<AnalyzedExpression>>,
    },
    While {
        condition: Box<AnalyzedExpression>,
        loop_body: Box<AnalyzedExpression>,
        else_expr: Option<Box<AnalyzedExpression>>,
    },
    Declaration {
        var_name: String,
        value: Box<AnalyzedExpression>,
    },
    ValueOfAssignable(AssignableExpression),
    Literal(AnalyzedLiteral),
    Unary {
        op: AnalyzedUnaryOp,
        expr: Box<AnalyzedExpression>,
    },
    Binary {
        op: AnalyzedBinaryOp,
        left: Box<AnalyzedExpression>,
        right: Box<AnalyzedExpression>,
    },
    Assign {
        op: BinaryAssignOp,
        lhs: AssignableExpression,
        rhs: Box<AnalyzedExpression>,
    },
    Borrow {
        expr: AssignableExpression,
    },
    FunctionCall {
        function_name: String,
        args: Vec<AnalyzedExpression>,
    },
    FieldAccess {
        expr: Box<AnalyzedExpression>,
        field_name: String,
    },
    ArrayIndex {
        array: Box<AnalyzedExpression>,
        index: Box<AnalyzedExpression>,
    },
    Increment(AssignableExpression, bool),
    Decrement(AssignableExpression, bool),
}

#[derive(Debug, Clone)]
pub enum AnalyzedLiteral {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
    Struct(Vec<(String, AnalyzedExpression)>),
    Array(Vec<AnalyzedExpression>),
}

#[derive(Debug, Clone)]
pub struct AssignableExpression {
    pub kind: AssignableExpressionKind,
    pub ty: AnalyzedType,
}
#[derive(Debug, Clone)]
pub enum AssignableExpressionKind {
    LocalVariable(String),
    Dereference(Box<AnalyzedExpression>),
    FieldAccess(Box<AssignableExpression>, String),
    ArrayIndex(Box<AnalyzedExpression>, Box<AnalyzedExpression>),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnalyzedBinaryOp {
    Math(BinaryMathOp),
    Logical(BinaryLogicOp),
    Comparison(BinaryComparisonOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryAssignOp {
    Assign,
    MathAssign(BinaryMathOp),
    LogicAssign(BinaryLogicOp),
}

#[derive(Debug, Clone)]
pub enum AnalyzedUnaryOp {
    Math(UnaryMathOp),
    LogicalNot,
    Cast,
}
