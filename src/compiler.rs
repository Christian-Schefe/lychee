use std::path::PathBuf;
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::pretty_print::PrettyPrint;

mod parser;
mod lexer;
mod codegen;

pub fn compile(source_input_path: &PathBuf, assembly_output_path: &PathBuf) {
    let tokens = lexer::lex(source_input_path);
    tokens.iter().for_each(|token| println!("{:?}", token));
    let program = parser::parse(TokenStack::new(tokens));
    println!("{}", program.pretty_print(0));
    let analyzed_program = parser::type_analyzer::analyze_program(&program).unwrap();
    codegen::generate_code(analyzed_program, assembly_output_path);
}