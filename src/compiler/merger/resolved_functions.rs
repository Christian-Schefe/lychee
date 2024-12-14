use crate::compiler::merger::function_collector::CollectedFunctionData;
use crate::compiler::merger::merged_expression::{FunctionId, ResolvedFunctionHeader, TypeId};
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::{ItemId, ParsedFunctionId};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedFunctions {
    pub functions: HashMap<ItemId, ResolvedFunctionHeader>,
    pub member_functions: HashMap<TypeId, HashMap<ItemId, ResolvedFunctionHeader>>,
    pub collected_function_data: CollectedFunctionData,
}

impl ResolvedFunctions {
    pub fn map_function_id(
        &self,
        function_id: &ParsedFunctionId,
        resolved_types: &ResolvedTypes,
    ) -> Option<FunctionId> {
        self.collected_function_data
            .map_function_id(function_id, resolved_types)
    }

    pub fn get_header(&self, function_id: &FunctionId) -> Option<&ResolvedFunctionHeader> {
        if let Some(impl_type) = &function_id.impl_type {
            self.member_functions
                .get(impl_type)
                .and_then(|map| map.get(&function_id.item_id))
        } else {
            self.functions.get(&function_id.item_id)
        }
    }
}
