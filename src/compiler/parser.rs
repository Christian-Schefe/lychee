use crate::compiler::parser::parsed_expression::ParsedProgram;
use crate::compiler::parser::program_parser::parse_module;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

mod binop_constructor;
mod binop_expr_parser;
pub mod expression_tree_printer;
pub mod parsed_expression;
mod parser_error;
mod primary_expr_parser;
mod program_parser;
mod type_parser;
mod unop_expr_parser;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ModuleIdentifier {
    pub path: Vec<String>,
    pub absolute: bool,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ModulePath {
    pub id: ModuleIdentifier,
    pub file: PathBuf,
}

impl ModulePath {
    pub fn get_submodule_path(&self, name: &str) -> ModulePath {
        let mut new_id = self.id.clone();
        new_id.path.push(name.to_string());
        let mut new_file = self.file.clone();
        new_file.set_file_name(format!("{}.lyc", name));
        if !new_file.try_exists().unwrap() {
            let stem = self.file.file_stem().unwrap().to_str().unwrap();
            new_file.pop();
            new_file.push(format!("{}/{}.lyc", stem, name));
        }
        ModulePath {
            id: new_id,
            file: new_file,
        }
    }
}

impl ModuleIdentifier {
    pub fn get_identifier(&self) -> String {
        let relative = self
            .path
            .iter()
            .map(|x| x.clone())
            .collect::<Vec<String>>()
            .join("::");
        if self.absolute {
            format!("::{}", relative)
        } else {
            relative
        }
    }
    pub fn resolve(&self, other: &ModuleIdentifier) -> ModuleIdentifier {
        if other.absolute {
            other.clone()
        } else {
            let mut new_path = self.path.clone();
            for name in &other.path {
                new_path.push(name.clone());
            }
            ModuleIdentifier {
                path: new_path,
                absolute: self.absolute,
            }
        }
    }
    pub fn len(&self) -> usize {
        self.path.len()
    }
}

pub fn parse(root_module_path: &PathBuf) -> ParsedProgram {
    let mut visited_modules = HashSet::new();
    let mut module_tree = HashMap::new();
    parse_module(
        &mut visited_modules,
        &mut module_tree,
        ModulePath {
            id: ModuleIdentifier {
                path: Vec::new(),
                absolute: true,
            },
            file: root_module_path.clone(),
        },
    )
    .unwrap();
    ParsedProgram { module_tree }
}
