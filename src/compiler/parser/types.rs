#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Byte,
    Char,
    Short,
    Int,
    Long,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::Bool => 1,
            Type::Byte => 1,
            Type::Char => 1,
            Type::Short => 2,
            Type::Int => 4,
            Type::Long => 8,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::Byte | Type::Short | Type::Int | Type::Long => true,
            _ => false,
        }
    }

    pub fn can_cast_to(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }
        match self {
            Type::Bool => matches!(other, Type::Byte | Type::Short | Type::Int | Type::Long),
            Type::Byte => matches!(other, Type::Char | Type::Bool | Type::Short | Type::Int | Type::Long),
            Type::Char => matches!(other, Type::Byte),
            Type::Short => matches!(other, Type::Bool | Type::Byte | Type::Int | Type::Long),
            Type::Int => matches!(other, Type::Bool | Type::Byte | Type::Short | Type::Long),
            Type::Long => matches!(other, Type::Bool | Type::Byte | Type::Short | Type::Int),
            _ => false,
        }
    }
}