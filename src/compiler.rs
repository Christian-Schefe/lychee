use std::path::PathBuf;
use crate::compiler::lexer::token_stack::TokenStack;

mod lexer;
mod parser;
mod analyzer;
mod resolver;
mod codegen;
mod builtin;

pub fn compile(source_input_path: &PathBuf, assembly_output_path: &PathBuf) {
    let tokens = lexer::lex(source_input_path).unwrap();
    tokens.iter().for_each(|token| println!("{:?}", token));
    let program = parser::parse(TokenStack::new(tokens));
    parser::expression_tree_printer::print_program(&program);
    let analyzed_program = analyzer::program_analyzer::analyze_program(&program).unwrap();
    let resolved_program = resolver::program_resolver::resolve_program(&analyzed_program);
    resolver::resolved_expression_printer::print_program(&resolved_program);
    codegen::gen_code(resolved_program, assembly_output_path);
}