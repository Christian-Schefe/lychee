use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::merger::merged_expression::ResolvedStruct;
use crate::compiler::merger::type_collector::CollectedTypeData;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{GenericParams, ParsedTypeKind};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedTypes {
    pub structs: HashMap<ItemId, ResolvedStruct>,
    pub collected_type_data: CollectedTypeData,
    pub generic_struct_instances: HashMap<ItemId, Vec<AnalyzedTypeId>>,
}

impl ResolvedTypes {
    pub fn resolve_type(&self, parsed_type: &ParsedTypeKind) -> Option<AnalyzedTypeId> {
        self.collected_type_data.map_parsed_type(parsed_type)
    }

    pub fn resolve_generic_type(
        &self,
        ty: &ParsedTypeKind,
        generic_args: &Option<GenericParams>,
    ) -> Option<AnalyzedTypeId> {
        let type_id = self
            .collected_type_data
            .map_generic_parsed_type(ty, generic_args);
        if let Some(AnalyzedTypeId::StructType(id, generic_args)) = &type_id {
            let resolved_struct = self.structs.get(&id)?;
            let generic_count = resolved_struct
                .generic_params
                .as_ref()
                .map(|x| x.order.len())
                .unwrap_or(0);
            if generic_args.len() != generic_count {
                return None;
            }
        }
        type_id
    }
    pub fn get_struct(&self, id: &AnalyzedTypeId) -> Option<&ResolvedStruct> {
        match id {
            AnalyzedTypeId::StructType(id, _) => self.structs.get(id),
            _ => None,
        }
    }
    pub fn get_pointer_struct(&self, id: &AnalyzedTypeId) -> Option<&ResolvedStruct> {
        match id {
            AnalyzedTypeId::StructType(id, _) => self.structs.get(id),
            AnalyzedTypeId::Pointer(inner) => self.get_pointer_struct(inner),
            _ => None,
        }
    }
}
