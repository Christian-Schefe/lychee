use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::merger::function_collector::CollectedFunctionData;
use crate::compiler::merger::merged_expression::{FunctionId, ResolvedFunctionHeader};
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::{ItemId, ParsedFunctionId};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedFunctions {
    pub functions: HashMap<ItemId, ResolvedFunctionHeader>,
    pub member_functions: HashMap<AnalyzedTypeId, HashMap<ItemId, ResolvedFunctionHeader>>,
    pub collected_function_data: CollectedFunctionData,
}

impl ResolvedFunctions {
    pub fn map_function_id(
        &self,
        function_id: &ParsedFunctionId,
        resolved_types: &ResolvedTypes,
    ) -> Option<FunctionId> {
        if let Some(impl_type) = &function_id.impl_type {
            let resolved_impl_type = resolved_types.resolve_type(&impl_type.value)?;
            self.collected_function_data
                .map_function_id(function_id, Some(&resolved_impl_type))
        } else {
            self.collected_function_data
                .map_function_id(function_id, None)
        }
    }

    pub fn map_member_function_id(
        &self,
        function_id: &ParsedFunctionId,
        impl_type: Option<&AnalyzedTypeId>,
    ) -> Option<FunctionId> {
        self.collected_function_data
            .map_function_id(function_id, impl_type)
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
