use std::collections::HashMap;
use crate::parser::syntax_tree::{SrcStatement, Type};

pub struct AnalyzedProgram {
    pub functions: HashMap<String, AnalyzedFunction>,
    pub main_function: String,
}

pub struct AnalyzedFunction {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub local_var_stack_size: usize,
    pub statements: Vec<SrcStatement>,
}