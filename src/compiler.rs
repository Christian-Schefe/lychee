use std::path::PathBuf;

mod analyzer;
mod builtin;
mod codegen;
mod lexer;
mod merger;
mod parser;
mod resolver;
mod unwrapper;

pub fn compile(source_input_path: &PathBuf, assembly_output_path: &PathBuf) {
    println!("Parsing...");
    let program = parser::parse(source_input_path);
    parser::expression_tree_printer::print_program(&program);
    println!("Merging...");
    let merged_program = merger::merge_program(&program);
    println!("Analyzing...");
    let analyzed_program = analyzer::program_analyzer::analyze_program(&merged_program).unwrap();
    analyzer::analyzed_expression_printer::print_program(&analyzed_program);
    println!("Unwrapping...");
    let unwrapped_program = unwrapper::unwrap_program(&analyzed_program);
    println!("Resolving...");
    let resolved_program = resolver::program_resolver::resolve_program(&unwrapped_program);
    resolver::resolved_expression_printer::print_program(&resolved_program);
    println!("Generating code...");
    codegen::gen_code(resolved_program, assembly_output_path);
}
