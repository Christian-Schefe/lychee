use crate::compiler::merger::merged_expression::{ResolvedStruct, TypeId};
use crate::compiler::merger::type_collector::CollectedTypeData;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::ParsedType;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedTypes {
    pub structs: HashMap<ItemId, ResolvedStruct>,
    pub type_sizes: HashMap<TypeId, usize>,
    pub collected_type_data: CollectedTypeData,
}

impl ResolvedTypes {
    pub fn get_type_size(&self, ty: &TypeId) -> usize {
        match ty {
            TypeId::Unit => 0,
            TypeId::Bool => 1,
            TypeId::Char => 1,
            TypeId::Integer(size) => *size,
            TypeId::Pointer(_) => 8,
            TypeId::StructType(_) => *self.type_sizes.get(ty).unwrap(),
        }
    }

    pub fn resolve_type(&self, parsed_type: &ParsedType) -> Option<TypeId> {
        self.collected_type_data.map_parsed_type(parsed_type)
    }

    pub fn get_struct(&self, id: &TypeId) -> Option<&ResolvedStruct> {
        match id {
            TypeId::StructType(id) => self.structs.get(id),
            _ => None,
        }
    }
    pub fn get_pointer_struct(&self, id: &TypeId) -> Option<&ResolvedStruct> {
        match id {
            TypeId::StructType(id) => self.structs.get(id),
            TypeId::Pointer(inner) => self.get_pointer_struct(inner),
            _ => None,
        }
    }
}
