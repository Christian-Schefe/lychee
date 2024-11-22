use std::collections::HashMap;
use lazy_static::lazy_static;
use crate::compiler::lexer::{HasLocation, Location};
use crate::compiler::lexer::token::StaticToken;
use crate::compiler::parser::types::Type;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<SrcFunction>,
}

#[derive(Debug, Clone)]
pub struct SrcFunction {
    pub(crate) function: Function,
    pub(crate) location: Location,
}

impl HasLocation for SrcFunction {
    fn location(&self) -> &Location {
        &self.location
    }
}

impl SrcFunction {
    pub fn new(function: Function, location: &Location) -> Self {
        Self {
            function,
            location: location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
    pub expr: SrcExpression,
}

#[derive(Debug, Clone)]
pub struct SrcStatement {
    pub(crate) statement: Statement,
    pub(crate) location: Location,
}

impl HasLocation for SrcStatement {
    fn location(&self) -> &Location {
        &self.location
    }
}

impl SrcStatement {
    pub fn new(statement: Statement, location: &Location) -> Self {
        Self {
            statement,
            location: location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<SrcExpression>),
    Declaration {
        var_type: Type,
        name: String,
        value: SrcExpression,
    },
    If {
        condition: SrcExpression,
        true_expr: SrcExpression,
        false_statement: Option<Box<SrcStatement>>,
    },
    For {
        init: Box<SrcStatement>,
        condition: SrcExpression,
        update: SrcExpression,
        body: SrcExpression,
    },
    While {
        condition: SrcExpression,
        body: SrcExpression,
        is_do_while: bool,
    },
    Expr(SrcExpression),
}

#[derive(Debug, Clone)]
pub struct SrcExpression {
    pub(crate) expr: Expression,
    pub(crate) location: Location,
}

impl HasLocation for SrcExpression {
    fn location(&self) -> &Location {
        &self.location
    }
}

impl SrcExpression {
    pub fn new(expr: Expression, location: &Location) -> Self {
        Self {
            expr,
            location: location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Block(Vec<SrcStatement>, Option<Box<SrcExpression>>),
    Binary {
        op: BinaryOp,
        left: Box<SrcExpression>,
        right: Box<SrcExpression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<SrcExpression>,
    },
    Literal(Literal),
    Variable(String),
    Ternary {
        condition: Box<SrcExpression>,
        true_expr: Box<SrcExpression>,
        false_expr: Box<SrcExpression>,
    },
    FunctionCall {
        function: String,
        args: Vec<SrcExpression>,
    },
    Cast {
        var_type: Type,
        expr: Box<SrcExpression>,
    },
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

lazy_static! {
    pub static ref ASSIGN_OP_MAP: HashMap<StaticToken, BinaryOp> = HashMap::from([
        (StaticToken::Assign, BinaryOp::Assign),
        (StaticToken::AddAssign, BinaryOp::MathAssign(BinaryMathOp::Add)),
        (StaticToken::SubAssign, BinaryOp::MathAssign(BinaryMathOp::Sub)),
        (StaticToken::MulAssign, BinaryOp::MathAssign(BinaryMathOp::Mul)),
        (StaticToken::DivAssign, BinaryOp::MathAssign(BinaryMathOp::Div)),
        (StaticToken::ModAssign, BinaryOp::MathAssign(BinaryMathOp::Mod)),
        (StaticToken::AndAssign, BinaryOp::MathAssign(BinaryMathOp::And)),
        (StaticToken::OrAssign, BinaryOp::MathAssign(BinaryMathOp::Or)),
        (StaticToken::XorAssign, BinaryOp::MathAssign(BinaryMathOp::Xor)),
        (StaticToken::ShiftLeftAssign, BinaryOp::MathAssign(BinaryMathOp::Shl)),
        (StaticToken::ShiftRightAssign, BinaryOp::MathAssign(BinaryMathOp::Shr)),
        (StaticToken::LogicalAndAssign, BinaryOp::LogicAssign(BinaryLogicOp::And)),
        (StaticToken::LogicalOrAssign, BinaryOp::LogicAssign(BinaryLogicOp::Or)),
    ]);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Positive,
    Negate,
    Not,
    LogicalNot,
    Increment,
    Decrement,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
}

impl Literal {
    pub fn get_type(&self) -> Type {
        match self {
            Literal::Unit => Type::Unit,
            Literal::Bool(_) => Type::Bool,
            Literal::Char(_) => Type::Char,
            Literal::Integer(_) => Type::Integer { size: 8 },
        }
    }
}
