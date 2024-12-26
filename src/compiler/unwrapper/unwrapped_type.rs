use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedBinaryOp, AnalyzedConstant, AnalyzedLiteral, AnalyzedUnaryOp, BinaryAssignOp,
};
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct UnwrappedProgram {
    pub structs: HashMap<String, UnwrappedStruct>,
    pub functions: HashMap<String, UnwrappedFunction>,
    pub main_function_name: String,
}

#[derive(Debug, Clone)]
pub struct UnwrappedFunction {
    pub name: String,
    pub body: UnwrappedExpression,
    pub return_type: UnwrappedTypeId,
    pub parameter_types: HashMap<String, UnwrappedTypeId>,
    pub parameter_order: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct UnwrappedStruct {
    pub field_types: HashMap<String, UnwrappedTypeId>,
    pub field_order: Vec<String>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum UnwrappedTypeId {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Pointer(Box<UnwrappedTypeId>),
    StructType(String),
    FunctionType(Box<UnwrappedTypeId>, Vec<UnwrappedTypeId>),
}

impl Display for UnwrappedTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnwrappedTypeId::Unit => write!(f, "unit"),
            UnwrappedTypeId::Bool => write!(f, "bool"),
            UnwrappedTypeId::Char => write!(f, "char"),
            UnwrappedTypeId::Integer(size) => match size {
                1 => write!(f, "byte"),
                2 => write!(f, "short"),
                4 => write!(f, "int"),
                8 => write!(f, "long"),
                _ => unreachable!("Invalid integer size: {}", size),
            },
            UnwrappedTypeId::Pointer(inner) => write!(f, "&{}", inner),
            UnwrappedTypeId::StructType(struct_id) => {
                write!(f, "{}", struct_id,)
            }
            UnwrappedTypeId::FunctionType(return_type, params) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnwrappedExpression {
    pub kind: UnwrappedExpressionKind,
    pub ty: UnwrappedTypeId,
}

#[derive(Debug, Clone)]
pub enum UnwrappedExpressionKind {
    Block {
        expressions: Vec<UnwrappedExpression>,
        returns_value: bool,
    },
    Return(Option<Box<UnwrappedExpression>>),
    Continue,
    Break(Option<Box<UnwrappedExpression>>),
    If {
        condition: Box<UnwrappedExpression>,
        then_block: Box<UnwrappedExpression>,
        else_expr: Option<Box<UnwrappedExpression>>,
    },
    Loop {
        init: Option<Box<UnwrappedExpression>>,
        condition: Option<Box<UnwrappedExpression>>,
        step: Option<Box<UnwrappedExpression>>,
        loop_body: Box<UnwrappedExpression>,
        else_expr: Option<Box<UnwrappedExpression>>,
    },
    Declaration {
        var_name: String,
        value: Box<UnwrappedExpression>,
    },
    ValueOfAssignable(AssignableUnwrappedExpression),
    Literal(AnalyzedLiteral),
    ConstantPointer(AnalyzedConstant),
    Unary {
        op: AnalyzedUnaryOp,
        expr: Box<UnwrappedExpression>,
    },
    Binary {
        op: AnalyzedBinaryOp,
        left: Box<UnwrappedExpression>,
        right: Box<UnwrappedExpression>,
    },
    Assign {
        op: BinaryAssignOp,
        lhs: AssignableUnwrappedExpression,
        rhs: Box<UnwrappedExpression>,
    },
    Borrow {
        expr: AssignableUnwrappedExpression,
    },
    FunctionCall {
        call_type: UnwrappedFunctionCallType,
        args: Vec<UnwrappedExpression>,
    },
    FieldAccess {
        expr: Box<UnwrappedExpression>,
        field_name: String,
    },
    Increment(AssignableUnwrappedExpression, bool),
    Decrement(AssignableUnwrappedExpression, bool),
    Sizeof(UnwrappedTypeId),
    StructInstance {
        fields: Vec<(String, UnwrappedExpression)>,
    },
    FunctionPointer(String),
}

#[derive(Debug, Clone)]
pub enum UnwrappedFunctionCallType {
    Function(String),
    Pointer(Box<UnwrappedExpression>),
}

#[derive(Debug, Clone)]
pub struct AssignableUnwrappedExpression {
    pub kind: AssignableUnwrappedExpressionKind,
    pub ty: UnwrappedTypeId,
}
#[derive(Debug, Clone)]
pub enum AssignableUnwrappedExpressionKind {
    LocalVariable(String),
    Dereference(Box<UnwrappedExpression>),
    FieldAccess(Box<AssignableUnwrappedExpression>, String),
    PointerFieldAccess(Box<UnwrappedExpression>, String, usize),
    ArrayIndex(Box<UnwrappedExpression>, Box<UnwrappedExpression>),
}
