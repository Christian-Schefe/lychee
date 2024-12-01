use crate::compiler::analyzer::AnalyzerResult;
use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::location::Location;
use crate::compiler::parser::parsed_expression::{ParsedProgram, ParsedType, ParsedTypeKind};
use anyhow::Context;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum AnalyzedType {
    Unit,
    Bool,
    Char,
    Integer(usize),
    Struct(String),
    Pointer(Box<AnalyzedType>),
}

impl Display for AnalyzedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalyzedType::Unit => write!(f, "unit"),
            AnalyzedType::Bool => write!(f, "bool"),
            AnalyzedType::Char => write!(f, "char"),
            AnalyzedType::Integer(size) => write!(f, "int{}", size * 8),
            AnalyzedType::Struct(name) => write!(f, "{}", name),
            AnalyzedType::Pointer(inner) => write!(f, "&{}", inner),
        }
    }
}

impl AnalyzedType {
    pub fn can_cast_to(&self, other: &AnalyzedType) -> bool {
        if self == other {
            return true;
        }
        match (self, other) {
            (AnalyzedType::Bool, AnalyzedType::Integer(_)) => true,
            (AnalyzedType::Integer(_), AnalyzedType::Bool) => true,
            (AnalyzedType::Char, AnalyzedType::Integer(_)) => true,
            (AnalyzedType::Integer(_), AnalyzedType::Char) => true,
            (AnalyzedType::Integer(_), AnalyzedType::Integer(_)) => true,
            (AnalyzedType::Pointer(_), AnalyzedType::Pointer(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnalyzedStructType {
    pub fields: HashMap<String, AnalyzedType>,
    pub field_order: Vec<String>,
    pub size: usize,
}

#[derive(Debug, Clone)]
pub struct AnalyzedTypes {
    pub struct_types: HashMap<String, AnalyzedStructType>,
    pub type_lookup: HashMap<String, AnalyzedType>,
}

impl AnalyzedTypes {
    pub fn resolve_type(&self, parsed_type: &ParsedType) -> AnalyzerResult<AnalyzedType> {
        match parsed_type {
            ParsedType {
                value: ParsedTypeKind::Named(name),
                ..
            } => Ok(self.type_lookup.get(name).cloned().ok_or_else(|| {
                LocationError::new(
                    format!("Unknown type: {}", name),
                    parsed_type.location.clone(),
                )
            })?),
            ParsedType {
                value: ParsedTypeKind::Pointer(inner),
                ..
            } => Ok(AnalyzedType::Pointer(Box::new(self.resolve_type(inner)?))),
        }
    }
}

pub fn analyze_types(program: &ParsedProgram) -> AnalyzerResult<AnalyzedTypes> {
    let builtin_types = [
        ("unit".to_string(), AnalyzedType::Unit),
        ("bool".to_string(), AnalyzedType::Bool),
        ("char".to_string(), AnalyzedType::Char),
        ("byte".to_string(), AnalyzedType::Integer(1)),
        ("short".to_string(), AnalyzedType::Integer(2)),
        ("int".to_string(), AnalyzedType::Integer(4)),
        ("long".to_string(), AnalyzedType::Integer(8)),
    ];
    let mut known_type_names =
        HashMap::with_capacity(builtin_types.len() + program.struct_definitions.len());
    for (name, analyzed_type) in builtin_types {
        known_type_names.insert(name, analyzed_type);
    }
    for struct_def in &program.struct_definitions {
        let name = struct_def.value.struct_name.clone();
        if let Some(other_type) =
            known_type_names.insert(name.clone(), AnalyzedType::Struct(name.clone()))
        {
            match other_type {
                AnalyzedType::Struct(_) => Err(LocationError::new(
                    format!("Duplicate struct definition: {}", name),
                    struct_def.location.clone(),
                ))?,
                _ => Err(LocationError::new(
                    format!("Struct cannot be named like a builtin type: {}", name),
                    struct_def.location.clone(),
                ))?,
            }
        }
    }

    let mut raw_struct_types = HashMap::with_capacity(program.struct_definitions.len());
    for struct_def in &program.struct_definitions {
        let mut fields = HashMap::with_capacity(struct_def.value.fields.len());
        let mut field_order = Vec::with_capacity(struct_def.value.fields.len());
        for (field_name, parsed_type) in &struct_def.value.fields {
            fields.insert(
                field_name.clone(),
                map_parsed_type(&known_type_names, parsed_type)?,
            );
            field_order.push(field_name.clone());
        }
        raw_struct_types.insert(
            struct_def.value.struct_name.clone(),
            (fields, field_order, struct_def.location.clone()),
        );
    }

    let mut struct_types = HashMap::with_capacity(raw_struct_types.len());
    for (name, (fields, field_order, location)) in &raw_struct_types {
        let mut visited = HashSet::new();
        let size = determine_struct_size(&raw_struct_types, name, &mut visited)
            .with_context(|| format!("Failed to analyze struct type '{name}' at {location}."))?;
        struct_types.insert(
            name.clone(),
            AnalyzedStructType {
                fields: fields.clone(),
                field_order: field_order.clone(),
                size,
            },
        );
    }

    Ok(AnalyzedTypes {
        struct_types,
        type_lookup: known_type_names,
    })
}

fn map_parsed_type(
    known_struct_names: &HashMap<String, AnalyzedType>,
    parsed_type: &ParsedType,
) -> AnalyzerResult<AnalyzedType> {
    match &parsed_type.value {
        ParsedTypeKind::Named(name) => {
            if let Some(analyzed_type) = known_struct_names.get(name) {
                Ok(analyzed_type.clone())
            } else {
                Err(LocationError::new(
                    format!("Unknown type: {}", name),
                    parsed_type.location.clone(),
                ))?
            }
        }
        ParsedTypeKind::Pointer(inner) => Ok(AnalyzedType::Pointer(Box::new(map_parsed_type(
            known_struct_names,
            inner,
        )?))),
    }
}

fn determine_struct_size(
    struct_types: &HashMap<String, (HashMap<String, AnalyzedType>, Vec<String>, Location)>,
    struct_name: &str,
    visited: &mut HashSet<String>,
) -> AnalyzerResult<usize> {
    let (struct_fields, _, location) = struct_types.get(struct_name).unwrap();
    if !visited.insert(struct_name.to_string()) {
        return Err(LocationError::new(
            format!("Struct {} contains a cycle", struct_name),
            location.clone(),
        ))?;
    }

    let mut struct_size = 0;
    for field_type in struct_fields.values() {
        match field_type {
            AnalyzedType::Unit => {}
            AnalyzedType::Bool => struct_size += 1,
            AnalyzedType::Char => struct_size += 1,
            AnalyzedType::Integer(size) => struct_size += size,
            AnalyzedType::Struct(name) => {
                struct_size += determine_struct_size(struct_types, name, visited)?
            }
            AnalyzedType::Pointer(_) => struct_size += 8,
        }
    }

    Ok(struct_size)
}
