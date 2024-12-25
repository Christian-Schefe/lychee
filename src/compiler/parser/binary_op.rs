#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Math(BinaryMathOp),
    Logical(BinaryLogicOp),
    Comparison(BinaryComparisonOp),
    Assign,
    MathAssign(BinaryMathOp),
    LogicAssign(BinaryLogicOp),
    Index,
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
            BinaryOp::Index => 120,
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
