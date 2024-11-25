use std::path::PathBuf;
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::pretty_print::PrettyPrint;

mod parser;
mod lexer;
mod codegen;
mod preprocessor;
mod parser2;

pub fn compile(source_input_path: &PathBuf, assembly_output_path: &PathBuf) {
    let tokens = lexer::lex(source_input_path).unwrap();
    tokens.iter().for_each(|token| println!("{:?}", token));
    let program = parser2::parse(TokenStack::new(tokens));
    parser2::expression_tree_printer::print_program(&program);
    //println!("{}", program.pretty_print(0));
    //let preprocessed_program = preprocessor::preprocess(program);
    //let analyzed_program = parser::type_analyzer::analyze_program(&preprocessed_program).unwrap();
    //codegen::generate_code(analyzed_program, assembly_output_path);
}