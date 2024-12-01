use crate::compiler::analyzer::type_resolver::{AnalyzedStructType, AnalyzedTypes};
use crate::compiler::resolver::expression_resolver::type_size_fn;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ResolvedStructType {
    pub field_offsets: HashMap<String, usize>,
    pub size: usize,
}

impl ResolvedStructType {
    pub fn new(
        analyzed_types: &AnalyzedTypes,
        struct_type: &AnalyzedStructType,
    ) -> ResolvedStructType {
        let mut field_offsets = HashMap::new();
        let mut offset = 0;
        for field_name in struct_type.field_order.iter().rev() {
            let field_type = struct_type.fields.get(field_name).unwrap();
            field_offsets.insert(field_name.clone(), offset);
            offset += type_size_fn(
                |x| analyzed_types.struct_types.get(x).unwrap().size,
                field_type,
            );
        }

        ResolvedStructType {
            field_offsets,
            size: offset,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedTypes {
    pub struct_types: HashMap<String, ResolvedStructType>,
}
