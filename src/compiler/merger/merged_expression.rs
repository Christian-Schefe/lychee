use crate::compiler::analyzer::analyzed_type::{AnalyzedTypeId, GenericParams};
use crate::compiler::analyzer::iterative_expression_analyzer::resolve_generic_type;
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::ItemId;
use std::collections::HashMap;
use std::fmt::Display;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
pub struct MergedProgram {
    pub resolved_functions: ResolvedFunctions,
    pub resolved_types: ResolvedTypes,
    pub root_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StructId {
    pub id: ItemId,
    pub generic_count: usize,
}

impl StructId {
    pub fn get_key(&self) -> String {
        format!("{};<{}>", self.id, self.generic_count)
    }
}

impl Display for StructId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if self.generic_count > 0 {
            write!(f, "<")?;
            for i in 0..self.generic_count {
                if i != 0 {
                    write!(f, ",")?;
                }
                write!(f, "_")?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedStruct {
    pub id: StructId,
    pub field_types: HashMap<String, AnalyzedTypeId>,
    pub field_order: Vec<String>,
    pub generic_params: GenericParams,
}

impl ResolvedStruct {
    pub fn get_field_type(
        &self,
        field_name: &str,
        generic_args: &Vec<AnalyzedTypeId>,
    ) -> Option<AnalyzedTypeId> {
        let field_type = self.field_types.get(field_name)?;
        Some(resolve_generic_type(
            field_type,
            &self.generic_params,
            generic_args,
        ))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedEnum {
    pub id: ItemId,
    pub variants: HashMap<String, i64>,
}

impl ResolvedEnum {
    pub fn get_variant_value(&self, variant_name: &str) -> Option<i64> {
        self.variants.get(variant_name).copied()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StructRef {
    pub id: StructId,
    pub generic_args: Vec<AnalyzedTypeId>,
}

impl Display for StructRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id.id)?;
        if self.id.generic_count != self.generic_args.len() {
            panic!(
                "Generic argument count mismatch: {} != {}",
                self.id.generic_count,
                self.generic_args.len()
            );
        }
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i != 0 {
                    write!(f, ",")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FunctionId {
    pub id: ItemId,
    pub body_index: isize,
    pub param_count: usize,
    pub generic_count: usize,
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if self.generic_count > 0 {
            write!(f, "<")?;
            for i in 0..self.generic_count {
                if i != 0 {
                    write!(f, ",")?;
                }
                write!(f, "_")?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for i in 0..self.param_count {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "_")?;
        }
        write!(f, ")")
    }
}

impl FunctionId {
    pub fn get_key(&self) -> String {
        format!(
            "{};{};<{}>;({})",
            self.id, self.body_index, self.generic_count, self.param_count
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedFunctionHeader {
    pub id: FunctionId,
    pub return_type: AnalyzedTypeId,
    pub parameter_types: HashMap<String, AnalyzedTypeId>,
    pub parameter_order: Vec<String>,
    pub generic_params: GenericParams,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionRef {
    pub id: FunctionId,
    pub generic_args: Vec<AnalyzedTypeId>,
    pub arg_types: Vec<AnalyzedTypeId>,
}

impl Display for FunctionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id.id)?;
        if self.id.generic_count != self.generic_args.len() {
            panic!(
                "Generic argument count mismatch: {} != {}",
                self.id.generic_count,
                self.generic_args.len()
            );
        }
        if self.id.param_count != self.arg_types.len() {
            panic!(
                "Argument count mismatch: {} != {}",
                self.id.param_count,
                self.arg_types.len()
            );
        }
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i != 0 {
                    write!(f, ",")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for (i, arg) in self.arg_types.iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}
