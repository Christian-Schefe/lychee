use crate::compiler::lexer::location::{Location, Src};

#[derive(Debug, Clone)]
pub struct ParsedProgram {
    pub functions: Vec<ParsedFunction>,
    pub struct_definitions: Vec<ParsedStructDefinition>,
}

#[derive(Debug, Clone)]
pub struct ParsedStructDefinition {
    pub struct_name: String,
    pub fields: Vec<(ParsedType, String)>,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
    pub function_name: String,
    pub return_type: ParsedType,
    pub args: Vec<(ParsedType, String)>,
    pub body: ParsedExpression,
    pub location: Location,
}

pub type ParsedExpression = Src<ParsedExpressionKind>;

#[derive(Debug, Clone)]
pub enum ParsedExpressionKind {
    Block(Vec<ParsedExpression>),
    Return(Option<Box<ParsedExpression>>),
    Continue,
    Break(Option<Box<ParsedExpression>>),
    If {
        condition: Box<ParsedExpression>,
        then_block: Box<ParsedExpression>,
        else_expr: Option<Box<ParsedExpression>>,
    },
    While {
        condition: Box<ParsedExpression>,
        loop_body: Box<ParsedExpression>,
        else_expr: Option<Box<ParsedExpression>>,
    },
    Declaration {
        var_type: ParsedType,
        var_name: String,
        value: Box<ParsedExpression>,
    },
    Variable(String),
    Literal(ParsedLiteral),
    Unary {
        op: UnaryOp,
        expr: Box<ParsedExpression>,
    },
    Binary {
        op: BinaryOp,
        left: Box<ParsedExpression>,
        right: Box<ParsedExpression>,
    },
    FunctionCall {
        function_name: String,
        args: Vec<ParsedExpression>,
    },
}

pub type ParsedType = Src<ParsedTypeKind>;

#[derive(Debug, Clone)]
pub enum ParsedTypeKind {
    Named(String),
    Pointer(Box<ParsedType>),
    Array(Box<ParsedType>),
}

#[derive(Debug, Clone)]
pub enum ParsedLiteral {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
    String(String),
    Struct(ParsedType, Vec<(String, ParsedExpression)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Math(BinaryMathOp),
    Logical(BinaryLogicOp),
    Comparison(BinaryComparisonOp),
    Assign,
    MathAssign(BinaryMathOp),
    LogicAssign(BinaryLogicOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryMathOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryLogicOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryComparisonOp {
    Equals,
    NotEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Math(UnaryMathOp),
    LogicalNot,
    Borrow,
    Dereference,
    Increment(bool),
    Decrement(bool),
    Cast(ParsedType),
    Member(String),
    Index(Box<ParsedExpression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryMathOp {
    Positive,
    Negate,
    BitwiseNot,
}