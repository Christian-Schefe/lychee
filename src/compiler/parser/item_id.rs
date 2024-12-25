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

impl ItemId {
    pub fn get_key(&self) -> String {
        format!("{}::{}", self.module_id.get_identifier(), self.item_name)
    }
}
