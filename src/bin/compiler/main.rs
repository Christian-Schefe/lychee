mod lexer;
mod parser;

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
    let program = parser::parse(TokenStack::new(tokens)).unwrap();
    println!("{:?}", program);
}