use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::analyzer::iterative_expression_analyzer::resolve_generic_type;
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::lexer::location::Location;
use crate::compiler::merger::function_collector::CollectedFunctionData;
use crate::compiler::merger::merged_expression::{FunctionId, FunctionRef, ResolvedFunctionHeader};
use crate::compiler::parser::item_id::ParsedFunctionId;
use crate::compiler::parser::parsed_expression::ParsedExpression;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedFunctions {
    pub function_headers: HashMap<FunctionId, ResolvedFunctionHeader>,
    pub function_bodies: Vec<(FunctionId, ParsedExpression)>,
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
        let function_ids = self
            .collected_function_data
            .find_function_id(function_id, arg_types.len(), generic_args.len())
            .ok_or_else(|| anyhow::anyhow!("Module not found"))?;

        if function_ids.len() == 0 {
            return Err(anyhow::anyhow!(
                "Function {} not found at {}",
                function_id.item_id,
                location
            ));
        }

        let mut matching_param_types = function_ids
            .into_iter()
            .map(|id| {
                let header = self.function_headers.get(&id).unwrap();

                for (arg_type, param_name) in arg_types.iter().zip(&header.parameter_order) {
                    let param_type = header.parameter_types.get(param_name).unwrap();
                    let actual_param_type =
                        resolve_generic_type(param_type, &header.generic_params, &generic_args);
                    if *arg_type != actual_param_type {
                        return Err(anyhow::anyhow!(
                            "Function '{}' call argument '{}' has type '{}', but expected '{}' at {}.",
                            function_id.item_id,
                            param_name,
                            arg_type,
                            actual_param_type,
                            location
                        ));
                    }
                }
                Ok(id)
            })
            .collect::<Vec<_>>();

        let mut valid_matching = matching_param_types
            .iter()
            .filter_map(|x| x.as_ref().cloned().ok())
            .collect::<Vec<_>>();

        match valid_matching.len() {
            0 => {
                if matching_param_types.len() == 1 {
                    matching_param_types.pop().unwrap()?;
                }
                return Err(anyhow::anyhow!(
                    "Function {} doesn't have matching parameters at {}",
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
            id: valid_matching.pop().unwrap(),
            arg_types: arg_types,
            generic_args: generic_args,
        })
    }

    pub fn get_header(&self, function_id: &FunctionId) -> Option<&ResolvedFunctionHeader> {
        self.function_headers.get(&function_id)
    }
}
