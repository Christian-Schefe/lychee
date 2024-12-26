use crate::compiler::parser::parsed_expression::ParsedType;
use crate::compiler::parser::ModuleIdentifier;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct ParsedScopeId {
    pub item_id: ItemId,
    pub is_module_local: bool,
}

impl Display for ParsedScopeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.item_id)
    }
}

#[derive(Debug, Clone)]
pub struct ParsedGenericId {
    pub id: ParsedScopeId,
    pub generic_args: Option<Vec<ParsedType>>,
}

impl Display for ParsedGenericId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(generic_args) = &self.generic_args {
            write!(f, "<")?;
            for (i, arg) in generic_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg.value)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemId {
    pub module_id: ModuleIdentifier,
    pub item_name: String,
}

impl Display for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.module_id.get_identifier(), self.item_name)
    }
}
