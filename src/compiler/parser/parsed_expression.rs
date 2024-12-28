use crate::compiler::lexer::location::Src;
use crate::compiler::parser::binary_op::BinaryOp;
use crate::compiler::parser::item_id::ParsedGenericId;
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::ModuleIdentifier;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct ParsedProgram {
    pub module_tree: HashMap<ModuleIdentifier, ParsedModule>,
    pub root_name: String,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub module_path: ModuleIdentifier,
    pub functions: Vec<Src<ParsedFunction>>,
    pub struct_definitions: Vec<Src<ParsedStructDefinition>>,
    pub type_aliases: Vec<Src<ParsedTypeAlias>>,
    pub imports: Vec<Src<ParsedImport>>,
    pub enums: Vec<Src<ParsedEnumDefinition>>,
    pub trait_definitions: Vec<Src<ParsedTraitDefinition>>,
    pub trait_implementations: Vec<Src<ParsedTraitImplementation>>,
}

#[derive(Debug, Clone)]
pub struct ParsedTypeAlias {
    pub alias: String,
    pub aliased_type: ParsedType,
}

#[derive(Debug, Clone)]
pub struct ParsedImport {
    pub imported_objects: Option<Vec<String>>,
    pub module_id: ModuleIdentifier,
}

#[derive(Debug, Clone)]
pub struct ParsedTraitDefinition {
    pub trait_name: String,
    pub functions: Vec<Src<ParsedFunctionSignature>>,
    pub generics: ParsedGenericParams,
}

#[derive(Debug, Clone)]
pub struct ParsedTraitImplementation {
    pub trait_id: ParsedGenericId,
    pub for_type: ParsedType,
    pub functions: Vec<Src<ParsedFunction>>,
}

#[derive(Debug, Clone)]
pub struct ParsedStructDefinition {
    pub struct_name: String,
    pub fields: Vec<(String, ParsedType)>,
    pub generics: ParsedGenericParams,
}

#[derive(Debug, Clone)]
pub struct ParsedEnumDefinition {
    pub enum_name: String,
    pub variants: Vec<(String, Option<ParsedLiteral>)>,
}

#[derive(Debug, Clone)]
pub struct ParsedGenericParams {
    pub order: Vec<String>,
}

impl ParsedGenericParams {
    pub fn new(order: Vec<String>) -> ParseResult<Self> {
        let mut set = HashSet::new();
        for generic in &order {
            if !set.insert(generic.clone()) {
                return Err(anyhow::anyhow!("Duplicate generic parameter: {}", generic))?;
            }
        }
        Ok(Self { order })
    }
    pub fn empty() -> Self {
        Self { order: Vec::new() }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedFunction {
    pub signature: ParsedFunctionSignature,
    pub body: ParsedExpression,
}

#[derive(Debug, Clone)]
pub struct ParsedFunctionSignature {
    pub function_name: String,
    pub return_type: ParsedType,
    pub generic_params: ParsedGenericParams,
    pub params: Vec<(ParsedType, String)>,
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
    Variable(ParsedGenericId),
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
        expr: Box<ParsedExpression>,
        args: Vec<ParsedExpression>,
    },
    Sizeof(ParsedType),
    StructInstance {
        struct_type: ParsedType,
        fields: Vec<(String, ParsedExpression)>,
    },
    Tuple(Vec<ParsedExpression>),
}

pub type ParsedType = Src<ParsedTypeKind>;

impl Display for ParsedTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ParsedTypeKind::Struct(id) => {
                write!(f, "{}", id.id.item_id)?;
                if let Some(generics) = &id.generic_args {
                    write!(f, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", generic.value)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            ParsedTypeKind::Pointer(inner) => write!(f, "&{}", inner),
            ParsedTypeKind::Unit => write!(f, "unit"),
            ParsedTypeKind::Bool => write!(f, "bool"),
            ParsedTypeKind::Char => write!(f, "char"),
            ParsedTypeKind::Integer(size) => match size {
                1 => write!(f, "byte"),
                2 => write!(f, "short"),
                4 => write!(f, "int"),
                8 => write!(f, "long"),
                _ => unreachable!(),
            },
            ParsedTypeKind::Function {
                return_type,
                params,
            } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.value)?;
                }
                write!(f, ") -> {}", return_type.value)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedTypeKind {
    Struct(ParsedGenericId),
    Pointer(Box<ParsedTypeKind>),
    Unit,
    Bool,
    Char,
    Integer(usize),
    Function {
        return_type: Box<ParsedType>,
        params: Vec<ParsedType>,
    },
}

#[derive(Debug, Clone)]
pub enum ParsedLiteral {
    Unit,
    Bool(bool),
    Char(i8),
    Integer(i64),
    String(String),
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
