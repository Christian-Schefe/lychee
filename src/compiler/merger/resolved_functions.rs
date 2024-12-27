use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::analyzer::iterative_expression_analyzer::{
    check_matches_try_find_generic_args, resolve_generic_type,
};
use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::lexer::location::Location;
use crate::compiler::merger::function_collector::CollectedFunctionData;
use crate::compiler::merger::merged_expression::{FunctionId, FunctionRef, ResolvedFunctionHeader};
use crate::compiler::parser::item_id::ParsedScopeId;
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
        function_id: &ParsedScopeId,
        arg_types: Vec<AnalyzedTypeId>,
        generic_args: Vec<AnalyzedTypeId>,
        location: &Location,
    ) -> AnalyzerResult<FunctionRef> {
        let function_ids = self.collected_function_data.find_function_id(
            function_id,
            Some(arg_types.len()),
            Some(generic_args.len()),
        );

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
                    "Function {} doesn't have matching parameters {} at {}",
                    function_id.item_id,
                    arg_types
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
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
            arg_types,
            generic_args,
        })
    }

    pub fn map_function_id_guess_generics(
        &self,
        function_id: &ParsedScopeId,
        arg_types: Vec<AnalyzedTypeId>,
        location: &Location,
    ) -> AnalyzerResult<FunctionRef> {
        let function_ids =
            self.collected_function_data
                .find_function_id(function_id, Some(arg_types.len()), None);

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

                let mut resolved_generic_map = HashMap::new();
                for (arg_type, param_name) in arg_types.iter().zip(&header.parameter_order) {
                    let param_type = header.parameter_types.get(param_name).unwrap();
                    check_matches_try_find_generic_args(
                        arg_type,
                        param_type,
                        &mut resolved_generic_map,
                        location
                    )?;
                }

                let mut generic_args = Vec::new();
                for generic_param in header.generic_params.get_all_ids() {
                    if let Some(generic_arg) = resolved_generic_map.get(&generic_param) {
                        generic_args.push(generic_arg.clone());
                    } else {
                        return Err(anyhow::anyhow!(
                            "Function '{}' call argument has unresolved generic type '{}' at {}.",
                            function_id.item_id,
                            generic_param,
                            location
                        ));
                    }
                }

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
                Ok((id, generic_args))
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
                    "Function {} with parameters '{}' doesn't match any function signature at {}",
                    function_id.item_id,
                    arg_types
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
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
        let (id, generic_args) = valid_matching.pop().unwrap();
        Ok(FunctionRef {
            id,
            arg_types,
            generic_args,
        })
    }

    pub fn function_id_from_scope_id(
        &self,
        function_id: &ParsedScopeId,
        generic_args: Vec<AnalyzedTypeId>,
        location: &Location,
    ) -> AnalyzerResult<FunctionRef> {
        let mut function_ids = self.collected_function_data.find_function_id(
            function_id,
            None,
            Some(generic_args.len()),
        );

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
        let id = function_ids.pop().unwrap();
        let header = self.function_headers.get(&id).unwrap();
        let arg_types = header
            .parameter_order
            .iter()
            .map(|x| header.parameter_types.get(x).unwrap().clone())
            .collect();
        let func_ref = FunctionRef {
            id,
            arg_types,
            generic_args,
        };
        Ok(func_ref)
    }

    pub fn get_header(&self, function_id: &FunctionId) -> &ResolvedFunctionHeader {
        self.function_headers.get(&function_id).unwrap()
    }
}
