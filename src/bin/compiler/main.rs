use crate::parser::pretty_print::PrettyPrint;
mod lexer;
mod parser;
mod codegen;

use std::path::PathBuf;
use clap::Parser;
use crate::lexer::token_stack::TokenStack;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: PathBuf,
    #[arg(short, long)]
    output: PathBuf,
}

fn main() {
    let args = Args::parse();
    let tokens = lexer::lex(args.input);
    tokens.iter().for_each(|token| println!("{:?}", token));
    let program = parser::parse(TokenStack::new(tokens));
    println!("{}", program.pretty_print(0));
    let analyzed_program = parser::type_analyzer::analyze_program(&program).unwrap();
    codegen::generate_code(analyzed_program, args.output);
}