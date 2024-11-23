use std::collections::HashMap;
use std::fmt::Display;
use crate::compiler::parser::syntax_tree::Src;

pub type SrcType = Src<UnknownType>;

#[derive(Debug, Clone)]
pub enum UnknownType {
    Named(String),
    Pointer(Box<SrcType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Char,
    Integer {
        size: usize
    },
    Pointer(Box<Type>),
    Struct {
        name: String,
        fields: HashMap<String, Type>,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Integer { size } => write!(f, "i{}", size * 8),
            Type::Pointer(ty) => write!(f, "&{}", ty),
            Type::Struct { name, fields } => {
                write!(f, "struct {} {{", name)?;
                for (i, (field_name, field_ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field_name, field_ty)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::Bool => 1,
            Type::Integer { size } => *size,
            Type::Char => 1,
            Type::Pointer(_) => 8,
            Type::Struct { fields, .. } => fields.iter().map(|(_, ty)| ty.size()).sum(),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::Integer { size: _ } => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn can_cast_to(&self, target: &Type) -> bool {
        if self == target || self.is_integer() && target.is_integer() {
            return true;
        }
        if let Type::Pointer(ty) = self {
            if let Type::Pointer(target_ty) = target {
                return (**ty == Type::Unit) || ty.size() >= target_ty.size();
            }
        }
        match self {
            Type::Bool => matches!(target, Type::Integer { size: _ }),
            Type::Char => matches!(target, Type::Integer { size: _ }),
            Type::Integer { size: _ } => matches!(target, Type::Bool | Type::Char),
            _ => false,
        }
    }
}