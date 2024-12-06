use crate::compiler::merger::merged_expression::{ResolvedStruct, ResolvedTypes};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedStructType {
    pub field_offsets: HashMap<String, usize>,
    pub size: usize,
}

impl ResolvedStructType {
    pub fn new(analyzed_types: &ResolvedTypes, struct_type: &ResolvedStruct) -> ResolvedStructType {
        let mut field_offsets = HashMap::new();
        let mut offset = 0;
        for field_name in struct_type.field_order.iter().rev() {
            let field_type = struct_type.field_types.get(field_name).unwrap();
            field_offsets.insert(field_name.clone(), offset);
            offset += analyzed_types.type_sizes.get(field_type).unwrap();
        }

        ResolvedStructType {
            field_offsets,
            size: offset,
        }
    }
}
