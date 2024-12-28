use crate::compiler::config::ConfigData;
use crate::compiler::parser::parsed_expression::ParsedProgram;
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::program_parser::parse_module;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

pub mod binary_op;
mod binop_constructor;
mod binop_expr_parser;
pub mod expression_tree_printer;
pub mod item_id;
pub mod parsed_expression;
mod parser_error;
mod parsing_utils;
mod primary_expr_parser;
mod program_parser;
mod type_parser;
mod unop_expr_parser;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ModuleIdentifier {
    pub root_name: String,
    pub path: Vec<String>,
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
    pub fn builtin() -> Self {
        ModuleIdentifier {
            root_name: "builtin".to_string(),
            path: vec![],
        }
    }
    
    pub fn get_identifier(&self) -> String {
        let path_str = self
            .path
            .iter()
            .map(|x| x.clone())
            .collect::<Vec<String>>()
            .join("::");
        format!("{}::{}", self.root_name, path_str)
    }
    pub fn resolve(&self, other: &Vec<String>, absolute: bool) -> ModuleIdentifier {
        if absolute {
            let root_name = other.first().unwrap().clone();
            let root_name = if root_name == "root" {
                self.root_name.clone()
            } else {
                root_name
            };
            ModuleIdentifier {
                path: other[1..].iter().cloned().collect(),
                root_name,
            }
        } else {
            let mut new_path = self.path.clone();
            for name in other {
                new_path.push(name.clone());
            }
            ModuleIdentifier {
                path: new_path,
                root_name: self.root_name.clone(),
            }
        }
    }
}

pub fn parse(entry_points: &Vec<ConfigData>) -> ParseResult<ParsedProgram> {
    let mut module_tree = HashMap::new();
    for entry_point in entry_points {
        let mut visited_modules = HashSet::new();
        parse_module(
            &mut visited_modules,
            &mut module_tree,
            ModulePath {
                id: ModuleIdentifier {
                    path: vec![],
                    root_name: entry_point.config.package.name.to_string(),
                },
                file: entry_point.get_entry_point()?,
            },
        )?;
    }

    let top_level_entry_point = entry_points.first().unwrap();

    Ok(ParsedProgram {
        module_tree,
        root_name: top_level_entry_point.config.package.name.to_string(),
    })
}
