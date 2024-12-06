use crate::compiler::parser::parsed_expression::ParsedProgram;
use crate::compiler::parser::program_parser::parse_module;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

mod binop_expr_parser;
pub mod expression_tree_printer;
pub mod parsed_expression;
mod parser_error;
mod primary_expr_parser;
mod program_parser;
mod type_parser;
mod unop_expr_parser;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ModulePath(pub Vec<String>);

impl ModulePath {
    pub fn get_path(&self, root_path: &PathBuf) -> PathBuf {
        let last = self.0.get(self.0.len() - 1).unwrap();
        if last == "root" {
            return root_path.clone();
        }
        root_path.with_file_name(format!("{}.lyc", last))
    }
    pub fn get_identifier(&self) -> String {
        self.0.join("::")
    }
    pub fn push(&mut self, name: String) {
        self.0.push(name);
    }
    pub fn resolve_relative(&self, relative: &ModulePath) -> ModulePath {
        let mut new_path = self.0.clone();
        for name in &relative.0 {
            new_path.push(name.clone());
        }
        ModulePath(new_path)
    }
    pub fn resolve(&self, other: &ModulePath) -> ModulePath {
        let is_absolute = other.0.len() > 0 && other.0[0] == "root";
        if is_absolute {
            ModulePath(other.0.clone())
        } else {
            self.resolve_relative(other)
        }
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

pub fn parse(root_module_path: &PathBuf) -> ParsedProgram {
    let mut visited_modules = HashSet::new();
    let mut module_tree = HashMap::new();
    parse_module(
        &mut visited_modules,
        &mut module_tree,
        root_module_path,
        ModulePath(vec!["root".to_string()]),
    )
    .unwrap();
    ParsedProgram { module_tree }
}
