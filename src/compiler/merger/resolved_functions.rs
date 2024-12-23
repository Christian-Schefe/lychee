use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::lexer::location::Location;
use crate::compiler::merger::function_collector::CollectedFunctionData;
use crate::compiler::merger::merged_expression::{FunctionId, FunctionRef, ResolvedFunctionHeader};
use crate::compiler::parser::item_id::ParsedFunctionId;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedFunctions {
    pub function_headers: HashMap<FunctionId, ResolvedFunctionHeader>,
    pub collected_function_data: CollectedFunctionData,
}

impl ResolvedFunctions {
    pub fn map_function_id(
        &self,
        function_id: &ParsedFunctionId,
        arg_types: Vec<AnalyzedTypeId>,
        generic_args: Vec<AnalyzedTypeId>,
        location: &Location,
    ) -> AnalyzerResult<FunctionRef> {
        let mut function_ids = self
            .collected_function_data
            .find_function_id(function_id, arg_types.len(), generic_args.len())
            .ok_or_else(|| anyhow::anyhow!("Module not found"))?;
        match function_ids.len() {
            0 => {
                return Err(anyhow::anyhow!(
                    "Function {} not found at {}",
                    function_id.item_id,
                    location
                ));
            }
            1 => {}
            _ => {
                return Err(anyhow::anyhow!(
                    "Ambiguous function call {} at {}",
                    function_id.item_id,
                    location
                ));
            }
        }
        Ok(FunctionRef {
            id: function_ids.pop().unwrap(),
            arg_types: arg_types,
            generic_args: generic_args,
        })
    }

    pub fn get_header(&self, function_id: &FunctionId) -> Option<&ResolvedFunctionHeader> {
        self.function_headers.get(&function_id)
    }
}
