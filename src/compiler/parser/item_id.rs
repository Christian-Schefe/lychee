use crate::compiler::parser::ModuleIdentifier;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct ParsedTypeId {
    pub item_id: ItemId,
    pub is_module_local: bool,
}

#[derive(Debug, Clone)]
pub struct ParsedFunctionId {
    pub item_id: ItemId,
    pub is_module_local: bool,
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
