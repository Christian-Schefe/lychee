use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::lexer::location::Src;
use crate::compiler::parser::item_id::{ParsedFunctionId, ParsedTypeId};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct ParsedProgram {
    pub module_tree: HashMap<ModuleIdentifier, ParsedModule>,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub module_path: ModuleIdentifier,
    pub functions: Vec<Src<ParsedFunction>>,
    pub struct_definitions: Vec<Src<ParsedStructDefinition>>,
    pub imports: Vec<Src<ParsedImport>>,
}

#[derive(Debug, Clone)]
pub struct ParsedImport {
    pub imported_object: Option<String>,
    pub module_id: ModuleIdentifier,
}

#[derive(Debug, Clone)]
pub struct ParsedStructDefinition {
    pub struct_name: String,
    pub fields: Vec<(String, ParsedType)>,
    pub generics: GenericParams,
}

#[derive(Debug, Clone)]
pub struct GenericParams {
    pub set: HashSet<String>,
    pub order: Vec<String>,
}

impl GenericParams {
    pub fn new(order: Vec<String>) -> ParseResult<Self> {
        let mut set = HashSet::new();
        for generic in &order {
            if !set.insert(generic.clone()) {
                return Err(anyhow::anyhow!("Duplicate generic parameter: {}", generic))?;
            }
        }
        Ok(Self { set, order })
    }
    pub fn empty() -> Self {
        Self {
            set: HashSet::new(),
            order: Vec::new(),
        }
    }
    pub fn resolve(
        &self,
        generic_name: &String,
        generic_args: &Vec<AnalyzedTypeId>,
    ) -> Option<AnalyzedTypeId> {
        let index = self.order.iter().position(|x| x == generic_name)?;
        Some(generic_args[index].clone())
    }

    pub fn get_generic(&self, generic_name: &String) -> Option<AnalyzedTypeId> {
        if !self.set.contains(generic_name) {
            return None;
        }
        Some(AnalyzedTypeId::GenericType(generic_name.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
    pub function_name: String,
    pub return_type: ParsedType,
    pub generic_params: GenericParams,
    pub params: Vec<(ParsedType, String)>,
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
        id: ParsedFunctionId,
        args: Vec<ParsedExpression>,
        generic_args: Vec<ParsedType>,
    },
}

pub type ParsedType = Src<ParsedTypeKind>;

impl Display for ParsedTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ParsedTypeKind::Struct(id, generics) => {
                write!(f, "{}<", id.item_id)?;
                for (i, generic) in generics.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", generic.value)?;
                }
                write!(f, ">")
            }
            ParsedTypeKind::Pointer(inner) => write!(f, "&{}", inner),
            ParsedTypeKind::Unit => write!(f, "unit"),
            ParsedTypeKind::Bool => write!(f, "bool"),
            ParsedTypeKind::Char => write!(f, "char"),
            ParsedTypeKind::Integer(size) => write!(f, "int{}", size),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedTypeKind {
    Struct(ParsedTypeId, Vec<ParsedType>),
    Pointer(Box<ParsedTypeKind>),
    Unit,
    Bool,
    Char,
    Integer(usize),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryMathOp {
    Positive,
    Negate,
    BitwiseNot,
}
