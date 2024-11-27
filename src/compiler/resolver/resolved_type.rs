use std::collections::HashMap;
use crate::compiler::analyzer::type_resolver::{AnalyzedStructType, AnalyzedType, AnalyzedTypes};
use crate::compiler::lexer::location::Location;
use crate::compiler::resolver::expression_resolver::{type_size_fn};

#[derive(Debug, Clone)]
pub struct ResolvedStructType {
    pub fields: HashMap<String, AnalyzedType>,
    pub field_order: Vec<String>,
    pub field_offsets: HashMap<String, usize>,
    pub size: usize,
    pub location: Location,
}

impl ResolvedStructType {
    pub fn new(analyzed_types: &AnalyzedTypes, struct_type: &AnalyzedStructType) -> ResolvedStructType {
        let mut field_offsets = HashMap::new();
        let mut offset = 0;
        for field_name in &struct_type.field_order {
            let field_type = struct_type.fields.get(field_name).unwrap();
            field_offsets.insert(field_name.clone(), offset);
            offset += type_size_fn(|x| analyzed_types.struct_types.get(x).unwrap().size, field_type);
        }

        ResolvedStructType {
            fields: struct_type.fields.clone(),
            field_order: struct_type.field_order.clone(),
            field_offsets,
            size: offset,
            location: struct_type.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedTypes {
    pub struct_types: HashMap<String, ResolvedStructType>,
}