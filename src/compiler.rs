use crate::compiler::config::ConfigData;
use std::path::PathBuf;

mod analyzer;
mod builtin;
mod codegen;
pub(crate) mod config;
mod lexer;
mod merger;
mod parser;
mod resolver;
mod unwrapper;

pub fn compile(
    config: ConfigData,
    output_dir: &PathBuf,
    debug_output: bool,
) -> Result<PathBuf, anyhow::Error> {
    let entry_points = config::find_all_entry_points(&config)?;

    println!("Parsing...");
    let program = parser::parse(&entry_points)?;
    if debug_output {
        let debug_ast_output =
            output_dir.join(format!("{}_parsed.debug", config.config.package.name));
        parser::expression_tree_printer::print_program(&program, &debug_ast_output);
    }

    println!("Merging...");
    let merged_program = merger::merge_program(&program);

    println!("Analyzing...");
    let analyzed_program = analyzer::program_analyzer::analyze_program(&merged_program)?;
    if debug_output {
        let debug_analyzed_output =
            output_dir.join(format!("{}_analyzed.debug", config.config.package.name));
        analyzer::analyzed_expression_printer::print_program(
            &analyzed_program,
            &debug_analyzed_output,
        );
    }

    println!("Unwrapping...");
    let unwrapped_program = unwrapper::unwrap_program(&analyzed_program);

    println!("Resolving...");
    let resolved_program = resolver::program_resolver::resolve_program(&unwrapped_program);
    if debug_output {
        let debug_resolved_output =
            output_dir.join(format!("{}_resolved.debug", config.config.package.name));
        resolver::resolved_expression_printer::print_program(
            &resolved_program,
            &debug_resolved_output,
        );
    }

    println!("Generating code...");
    let assembly_output = output_dir.join(format!("{}.bud", config.config.package.name));
    codegen::gen_code(resolved_program, &assembly_output);
    Ok(assembly_output)
}
