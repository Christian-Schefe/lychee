use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{GenericParams, ParsedExpression};
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct MergedProgram {
    pub function_bodies: Vec<(FunctionId, ParsedExpression)>,
    pub resolved_functions: ResolvedFunctions,
    pub resolved_types: ResolvedTypes,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunctionHeader {
    pub return_type: AnalyzedTypeId,
    pub parameter_types: HashMap<String, AnalyzedTypeId>,
    pub parameter_order: Vec<String>,
    pub generic_params: Option<GenericParams>,
}

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    pub id: ItemId,
    pub field_types: HashMap<String, AnalyzedTypeId>,
    pub field_order: Vec<String>,
    pub generic_params: Option<GenericParams>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId {
    pub item_id: ItemId,
    pub impl_type: Option<AnalyzedTypeId>,
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.impl_type {
            Some(impl_type) => write!(f, "{}@{}", self.item_id, impl_type),
            None => write!(f, "{}", self.item_id),
        }
    }
}
