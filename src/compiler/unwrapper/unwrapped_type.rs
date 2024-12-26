use crate::compiler::analyzer::analyzed_expression::{
    AnalyzedBinaryOp, AnalyzedConstant, AnalyzedLiteral, AnalyzedUnaryOp, BinaryAssignOp,
};
use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::merger::merged_expression::{FunctionId, ResolvedStruct, StructId};
use std::collections::HashMap;

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

impl UnwrappedTypeId {
    pub fn upgrade_no_generic(type_id: &AnalyzedTypeId) -> Self {
        match type_id {
            AnalyzedTypeId::Unit => UnwrappedTypeId::Unit,
            AnalyzedTypeId::Bool => UnwrappedTypeId::Bool,
            AnalyzedTypeId::Char => UnwrappedTypeId::Char,
            AnalyzedTypeId::Integer(size) => UnwrappedTypeId::Integer(*size),
            AnalyzedTypeId::Pointer(inner) => {
                UnwrappedTypeId::Pointer(Box::new(Self::upgrade_no_generic(inner)))
            }
            AnalyzedTypeId::StructType(_) => unimplemented!(),
            AnalyzedTypeId::FunctionType(return_type, params) => UnwrappedTypeId::FunctionType(
                Box::new(Self::upgrade_no_generic(return_type)),
                params.iter().map(|x| Self::upgrade_no_generic(x)).collect(),
            ),
            AnalyzedTypeId::GenericType(_) => unreachable!(),
            AnalyzedTypeId::EnumType(_) => unreachable!(),
        }
    }
    pub fn get_key(&self) -> String {
        match self {
            UnwrappedTypeId::Unit => "unit".to_string(),
            UnwrappedTypeId::Bool => "bool".to_string(),
            UnwrappedTypeId::Char => "char".to_string(),
            UnwrappedTypeId::Integer(size) => format!("int{}", size),
            UnwrappedTypeId::Pointer(inner) => format!("&{}", inner.get_key()),
            UnwrappedTypeId::StructType(id) => format!("Struct({})", id),
            UnwrappedTypeId::FunctionType(return_type, params) => {
                let mut key = format!("fn(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        key.push_str(",");
                    }
                    key.push_str(&param.get_key());
                }
                key.push_str(")->");
                key.push_str(&return_type.get_key());
                key
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnwrappedFunctionRef {
    pub id: FunctionId,
    pub generic_args: Vec<UnwrappedTypeId>,
    pub arg_types: Vec<UnwrappedTypeId>,
}

impl UnwrappedFunctionRef {
    pub fn get_key(&self) -> String {
        format!(
            "{};<{}>;({})",
            self.id.get_key(),
            self.generic_args
                .iter()
                .map(|x| x.get_key())
                .collect::<Vec<_>>()
                .join(","),
            self.arg_types
                .iter()
                .map(|x| x.get_key())
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

#[derive(Debug, Clone)]
pub struct UnwrappedStructRef<'a> {
    pub id: StructId,
    pub generic_args: Vec<UnwrappedTypeId>,
    pub struct_def: &'a ResolvedStruct,
}

impl<'a> UnwrappedStructRef<'a> {
    pub fn get_key(&self) -> String {
        format!(
            "{};<{}>",
            self.id.get_key(),
            self.generic_args
                .iter()
                .map(|x| x.get_key())
                .collect::<Vec<_>>()
                .join(",")
        )
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
