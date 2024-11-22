use std::collections::HashMap;
use crate::compiler::parser::syntax_tree::SrcExpression;
use crate::compiler::parser::types::Type;

pub struct AnalyzedProgram {
    pub functions: HashMap<String, AnalyzedFunction>,
    pub main_function: String,
}

pub struct AnalyzedFunction {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
    pub local_var_stack_size: usize,
    pub expr: SrcExpression,
}