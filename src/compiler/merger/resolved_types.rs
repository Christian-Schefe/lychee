use crate::compiler::analyzer::analyzed_type::{AnalyzedTypeId, GenericParams};
use crate::compiler::merger::merged_expression::{
    ResolvedEnum, ResolvedStruct, StructId, StructRef,
};
use crate::compiler::merger::type_collector::CollectedTypeData;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::ParsedTypeKind;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedTypes {
    pub structs: HashMap<StructId, ResolvedStruct>,
    pub enums: HashMap<ItemId, ResolvedEnum>,
    pub collected_type_data: CollectedTypeData,
}

impl ResolvedTypes {
    pub fn map_generic_parsed_type(
        &self,
        ty: &ParsedTypeKind,
        generic_args: &GenericParams,
    ) -> Option<AnalyzedTypeId> {
        self.collected_type_data
            .map_generic_parsed_type(ty, generic_args)
    }
    pub fn get_struct(&self, struct_ref: &StructRef) -> Option<&ResolvedStruct> {
        self.structs.get(&struct_ref.id)
    }
    pub fn get_struct_from_type(&self, id: &AnalyzedTypeId) -> Option<&ResolvedStruct> {
        match id {
            AnalyzedTypeId::StructType(struct_ref) => self.structs.get(&struct_ref.id),
            _ => None,
        }
    }
    pub fn get_enum_from_variant(&self, variant_id: &ItemId) -> Option<&ResolvedEnum> {
        let mut enum_id = ItemId {
            item_name: variant_id.module_id.path.last().unwrap().clone(),
            module_id: variant_id.module_id.clone(),
        };
        enum_id.module_id.path.pop();
        self.enums.get(&enum_id)
    }
}
