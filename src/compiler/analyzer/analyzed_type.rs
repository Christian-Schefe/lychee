use crate::compiler::merger::merged_expression::{FunctionId, StructId, StructRef};
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::ParsedGenericParams;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum AnalyzedTypeId {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Pointer(Box<AnalyzedTypeId>),
    StructType(StructRef),
    EnumType(ItemId),
    GenericType(GenericId),
    FunctionType(Box<AnalyzedTypeId>, Vec<AnalyzedTypeId>),
}

impl Display for AnalyzedTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalyzedTypeId::Unit => write!(f, "unit"),
            AnalyzedTypeId::Bool => write!(f, "bool"),
            AnalyzedTypeId::Char => write!(f, "char"),
            AnalyzedTypeId::Integer(size) => match size {
                1 => write!(f, "byte"),
                2 => write!(f, "short"),
                4 => write!(f, "int"),
                8 => write!(f, "long"),
                _ => unreachable!("Invalid integer size: {}", size),
            },
            AnalyzedTypeId::Pointer(inner) => write!(f, "&{}", inner),
            AnalyzedTypeId::StructType(struct_id) => {
                write!(f, "{}", struct_id)
            }
            AnalyzedTypeId::EnumType(id) => {
                write!(f, "{}", id)
            }
            AnalyzedTypeId::GenericType(id) => {
                write!(f, "{}", id)
            }
            AnalyzedTypeId::FunctionType(return_type, params) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ")->{}", return_type)
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericParams {
    mapping: HashMap<String, usize>,
    kind: GenericIdKind,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct GenericId {
    pub kind: GenericIdKind,
    pub index: usize,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub enum GenericIdKind {
    Struct(StructId),
    Function(FunctionId),
}
impl Display for GenericIdKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            GenericIdKind::Struct(id) => write!(f, "{}", id),
            GenericIdKind::Function(id) => write!(f, "{}", id),
        }
    }
}

impl Display for GenericId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}> of {}", self.index, self.kind)
    }
}

impl GenericParams {
    pub fn empty(kind: GenericIdKind) -> Self {
        Self {
            mapping: HashMap::new(),
            kind,
        }
    }
    pub fn from_order(kind: GenericIdKind, order: Vec<String>) -> Self {
        let mut mapping = HashMap::new();
        for (i, name) in order.iter().enumerate() {
            mapping.insert(name.clone(), i);
        }
        Self { mapping, kind }
    }
    pub fn from(kind: GenericIdKind, params: &ParsedGenericParams) -> Self {
        let mut mapping = HashMap::new();
        for (i, param) in params.order.iter().enumerate() {
            mapping.insert(param.clone(), i);
        }
        Self { mapping, kind }
    }
    pub fn resolve<T: Clone>(&self, generic_name: &GenericId, generic_args: &Vec<T>) -> Option<T> {
        if self.kind != generic_name.kind {
            return None;
        }
        Some(generic_args[generic_name.index].clone())
    }

    pub fn get_all_ids(&self) -> Vec<GenericId> {
        self.mapping
            .iter()
            .map(|(_, index)| GenericId {
                kind: self.kind.clone(),
                index: *index,
            })
            .collect()
    }

    pub fn get_generic_from_name(&self, generic_name: &String) -> Option<GenericId> {
        if let Some(index) = self.mapping.get(generic_name) {
            Some(GenericId {
                kind: self.kind.clone(),
                index: *index,
            })
        } else {
            None
        }
    }
}
