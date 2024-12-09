use crate::compiler::lexer::location::Src;
use crate::compiler::merger::merged_expression::ModuleId;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ParsedProgram {
    pub module_tree: HashMap<ModuleIdentifier, ParsedModule>,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub module_path: ModuleIdentifier,
    pub functions: Vec<Src<ParsedFunction>>,
    pub struct_definitions: Vec<Src<ParsedStructDefinition>>,
    pub imports: HashMap<String, Src<ModuleId>>,
}

#[derive(Debug, Clone)]
pub struct ParsedStructDefinition {
    pub struct_name: String,
    pub fields: Vec<(String, ParsedType)>,
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
    pub function_name: String,
    pub return_type: ParsedType,
    pub args: Vec<(ParsedType, String)>,
    pub body: ParsedExpression,
}

pub type ParsedExpression = Src<ParsedExpressionKind>;

#[derive(Debug, Clone)]
pub enum ParsedExpressionKind {
    Block {
        expressions: Vec<ParsedExpression>,
        returns_value: bool,
    },
    Return(Option<Box<ParsedExpression>>),
    Continue,
    Break(Option<Box<ParsedExpression>>),
    If {
        condition: Box<ParsedExpression>,
        then_block: Box<ParsedExpression>,
        else_expr: Option<Box<ParsedExpression>>,
    },
    Loop {
        init: Option<Box<ParsedExpression>>,
        condition: Option<Box<ParsedExpression>>,
        step: Option<Box<ParsedExpression>>,
        loop_body: Box<ParsedExpression>,
        else_expr: Option<Box<ParsedExpression>>,
    },
    Declaration {
        var_type: Option<ParsedType>,
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
        function_id: ModuleId,
        args: Vec<ParsedExpression>,
    },
}

pub type ParsedType = Src<ParsedTypeKind>;

#[derive(Debug, Clone)]
pub enum ParsedTypeKind {
    Named(ModuleId),
    Pointer(Box<ParsedType>),
}

#[derive(Debug, Clone)]
pub enum ParsedLiteral {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
    String(String),
    Struct(ParsedType, HashMap<String, ParsedExpression>),
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

impl BinaryOp {
    pub fn precedence(&self) -> usize {
        match self {
            BinaryOp::Math(BinaryMathOp::Mul)
            | BinaryOp::Math(BinaryMathOp::Div)
            | BinaryOp::Math(BinaryMathOp::Mod) => 10,

            BinaryOp::Math(BinaryMathOp::Add) | BinaryOp::Math(BinaryMathOp::Sub) => 20,

            BinaryOp::Math(BinaryMathOp::Shl) | BinaryOp::Math(BinaryMathOp::Shr) => 30,

            BinaryOp::Comparison(BinaryComparisonOp::Less)
            | BinaryOp::Comparison(BinaryComparisonOp::LessEquals)
            | BinaryOp::Comparison(BinaryComparisonOp::Greater)
            | BinaryOp::Comparison(BinaryComparisonOp::GreaterEquals) => 40,

            BinaryOp::Comparison(BinaryComparisonOp::Equals)
            | BinaryOp::Comparison(BinaryComparisonOp::NotEquals) => 50,

            BinaryOp::Math(BinaryMathOp::And) => 60,

            BinaryOp::Math(BinaryMathOp::Or) => 70,

            BinaryOp::Math(BinaryMathOp::Xor) => 80,

            BinaryOp::Logical(BinaryLogicOp::And) => 90,

            BinaryOp::Logical(BinaryLogicOp::Or) => 100,

            BinaryOp::Assign => 110,
            BinaryOp::MathAssign(_) => 110,
            BinaryOp::LogicAssign(_) => 110,
        }
    }
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
    Increment { is_prefix: bool },
    Decrement { is_prefix: bool },
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
