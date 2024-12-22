use crate::compiler::analyzer::analyzed_type::AnalyzedTypeId;
use crate::compiler::analyzer::iterative_expression_analyzer::resolve_generic_type;
use crate::compiler::merger::resolved_functions::ResolvedFunctions;
use crate::compiler::merger::resolved_types::ResolvedTypes;
use crate::compiler::parser::item_id::ItemId;
use crate::compiler::parser::parsed_expression::{GenericParams, ParsedExpression};
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct MergedProgram {
    pub function_bodies: Vec<(FunctionId, ParsedExpression)>,
    pub resolved_functions: ResolvedFunctions,
    pub resolved_types: ResolvedTypes,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructId {
    pub id: ItemId,
    pub generic_count: usize,
}

impl Display for StructId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if self.generic_count > 0 {
            write!(f, "<")?;
            for i in 0..self.generic_count {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "_")?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructRef {
    pub id: StructId,
    pub generic_args: Vec<AnalyzedTypeId>,
}

impl Display for StructRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId {
    pub id: ItemId,
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
                    write!(f, ", ")?;
                }
                write!(f, "_")?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for i in 0..self.param_count {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "_")?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
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
    pub arg_types: Vec<AnalyzedTypeId>,
    pub generic_args: Vec<AnalyzedTypeId>,
}

impl Display for FunctionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, ">")?;
        }
        write!(f, "(")?;
        for (i, arg) in self.arg_types.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}
