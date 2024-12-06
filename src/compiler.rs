use std::path::PathBuf;

mod analyzer;
mod builtin;
mod codegen;
mod lexer;
mod merger;
mod parser;
mod resolver;

pub fn compile(source_input_path: &PathBuf, assembly_output_path: &PathBuf) {
    let program = parser::parse(source_input_path);
    parser::expression_tree_printer::print_program(&program);
    let merged_program = merger::merge_program(&program);
    let analyzed_program = analyzer::program_analyzer::analyze_program(&merged_program).unwrap();
    let resolved_program = resolver::program_resolver::resolve_program(&analyzed_program);
    resolver::resolved_expression_printer::print_program(&resolved_program);
    codegen::gen_code(resolved_program, assembly_output_path);
}
