use crate::compiler::merger::merged_expression::StructRef;
use std::fmt::Display;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum AnalyzedTypeId {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Pointer(Box<AnalyzedTypeId>),
    StructType(StructRef),
    GenericType(String),
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
            AnalyzedTypeId::GenericType(name) => write!(f, "{}", name),
        }
    }
}
