#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Option<Expression>),
    Declaration {
        var_type: Type,
        name: String,
        value: Option<Expression>,
    },
    Expr(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Block(Vec<Statement>, Option<Box<Expression>>),
    Call {
        function: String,
        arguments: Vec<Expression>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Literal(Literal),
    Variable(String),
    Ternary {
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        false_expr: Box<Expression>,
    },
    FunctionCall {
        function: String,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    LogicalAnd,
    LogicalOr,
    Shl,
    Shr,
    Equals,
    NotEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug)]
pub enum UnaryOp {
    Positive,
    Negate,
    Not,
    LogicalNot,
}

#[derive(Debug)]
pub enum Literal {
    Int(i32),
    Long(i64),
    String(String),
}

#[derive(Debug)]
pub enum Type {
    Unit,
    Int,
    Long,
    String,
}