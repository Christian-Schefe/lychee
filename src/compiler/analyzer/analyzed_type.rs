use crate::compiler::merger::merged_expression::{FunctionId, StructId, StructRef};
use crate::compiler::parser::parsed_expression::ParsedGenericParams;
use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::hash::Hash;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum AnalyzedTypeId {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Pointer(Box<AnalyzedTypeId>),
    StructType(StructRef),
    GenericType(GenericId),
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
            AnalyzedTypeId::GenericType(id) => {
                write!(f, "{}", id)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericParams {
    set: HashSet<String>,
    pub order: Vec<String>,
    kind: GenericIdKind,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct GenericId {
    pub kind: GenericIdKind,
    pub name: String,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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
        write!(f, "<{}> of {}", self.name, self.kind)
    }
}

impl GenericParams {
    pub fn empty(kind: GenericIdKind) -> Self {
        Self {
            set: HashSet::new(),
            order: Vec::new(),
            kind,
        }
    }
    pub fn from(kind: GenericIdKind, params: &ParsedGenericParams) -> Self {
        let mut set = HashSet::new();
        let mut order = Vec::new();
        for param in &params.order {
            set.insert(param.clone());
            order.push(param.clone());
        }
        Self { set, order, kind }
    }
    pub fn resolve(
        &self,
        generic_name: &GenericId,
        generic_args: &Vec<AnalyzedTypeId>,
    ) -> Option<AnalyzedTypeId> {
        if self.kind != generic_name.kind {
            return None;
        }
        let index = self.order.iter().position(|id| *id == *generic_name.name)?;
        Some(generic_args[index].clone())
    }

    pub fn get_generic_from_name(&self, generic_name: &String) -> Option<GenericId> {
        if self.set.contains(generic_name) {
            Some(GenericId {
                kind: self.kind.clone(),
                name: generic_name.clone(),
            })
        } else {
            None
        }
    }
}
