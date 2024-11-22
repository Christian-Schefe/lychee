#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Char,
    Integer {
        size: usize
    },
    Pointer(Box<Type>),
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Unit => 0,
            Type::Bool => 1,
            Type::Integer { size } => *size,
            Type::Char => 1,
            Type::Pointer(_) => 8,
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
        match self {
            Type::Bool => matches!(target, Type::Integer { size: _ }),
            Type::Char => matches!(target, Type::Integer { size: _ }),
            Type::Integer { size: _ } => matches!(target, Type::Bool | Type::Char),
            _ => false,
        }
    }
}