use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser2::parsed_expression::ParsedProgram;
use crate::compiler::parser2::program_parser::parse_program;

mod parsed_expression;
mod program_parser;
mod parser_error;
mod type_parser;
pub mod expression_tree_printer;
mod binop_expr_parser;
mod unop_expr_parser;
mod primary_expr_parser;

pub fn parse(mut tokens: TokenStack) -> ParsedProgram {
    let program = parse_program(&mut tokens).unwrap();
    program
}