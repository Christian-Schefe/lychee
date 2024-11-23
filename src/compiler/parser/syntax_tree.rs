use crate::compiler::lexer::{HasLocation, Location};
use crate::compiler::parser::types::{Type, SrcType};

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<SrcFunction>,
    pub struct_definitions: Vec<SrcStructDefinition>,
}

pub type SrcStructDefinition = Src<StructDefinition>;

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub name: String,
    pub fields: Vec<(String, SrcType)>,
}

pub type SrcFunction = Src<Function>;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, SrcType)>,
    pub return_type: SrcType,
    pub expr: SrcExpression,
}


pub type SrcStatement = Src<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<SrcExpression>),
    Declaration {
        var_type: SrcType,
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

pub type SrcExpression = Src<Expression>;

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
    Sizeof(SrcType),
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
        var_type: SrcType,
        expr: Box<SrcExpression>,
    },
    StructLiteral {
        struct_type: SrcType,
        fields: Vec<(String, SrcExpression)>,
    },
    MemberAccess {
        expr: Box<SrcExpression>,
        member: String,
    },
    Increment {
        expr: Box<SrcExpression>,
        is_increment: bool,
        postfix: bool,
    },
    Borrow(Box<SrcExpression>),
    Dereference(Box<SrcExpression>),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Positive,
    Negate,
    Not,
    LogicalNot,
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

#[derive(Debug, Clone)]
pub struct Src<T> {
    pub(crate) value: T,
    pub(crate) location: Location,
}

impl<T> HasLocation for Src<T> {
    fn location(&self) -> &Location {
        &self.location
    }
}

impl<T> Src<T> {
    pub fn new(value: T, location: &Location) -> Self {
        Self {
            value,
            location: location.clone(),
        }
    }
}