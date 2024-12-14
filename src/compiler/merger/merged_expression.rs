use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::ParsedExpression;
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct MergedProgram {
    pub function_bodies: HashMap<FunctionId, ParsedExpression>,
    pub resolved_functions: ResolvedFunctions,
    pub resolved_types: ResolvedTypes,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunctionHeader {
    pub return_type: TypeId,
    pub parameter_types: HashMap<String, TypeId>,
    pub parameter_order: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    pub id: ItemId,
    pub field_types: HashMap<String, TypeId>,
    pub field_order: Vec<String>,
    pub field_offsets: HashMap<String, usize>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TypeId {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Pointer(Box<TypeId>),
    StructType(ItemId),
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeId::Unit => write!(f, "unit"),
            TypeId::Bool => write!(f, "bool"),
            TypeId::Char => write!(f, "char"),
            TypeId::Integer(size) => match size {
                1 => write!(f, "byte"),
                2 => write!(f, "short"),
                4 => write!(f, "int"),
                8 => write!(f, "long"),
                _ => unreachable!("Invalid integer size: {}", size),
            },
            TypeId::Pointer(inner) => write!(f, "&{}", inner),
            TypeId::StructType(module_id) => write!(f, "{}", module_id),
        }
    }
}

impl TypeId {
    pub fn type_name(&self) -> String {
        match self {
            TypeId::Unit => "unit".to_string(),
            TypeId::Bool => "bool".to_string(),
            TypeId::Char => "char".to_string(),
            TypeId::Integer(size) => match size {
                1 => "byte".to_string(),
                2 => "short".to_string(),
                4 => "int".to_string(),
                8 => "long".to_string(),
                _ => unreachable!("Invalid integer size: {}", size),
            },
            TypeId::Pointer(inner) => format!("&{}", inner.type_name()),
            TypeId::StructType(module_id) => module_id.item_name.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId {
    pub item_id: ItemId,
    pub impl_type: Option<TypeId>,
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.impl_type {
            Some(impl_type) => write!(f, "{}@{}", self.item_id, impl_type),
            None => write!(f, "{}", self.item_id),
        }
    }
}
