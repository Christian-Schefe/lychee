use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parsed_expression::ParsedProgram;
use crate::compiler::parser::program_parser::parse_program;

mod binop_expr_parser;
pub mod expression_tree_printer;
pub mod parsed_expression;
mod parser_error;
mod primary_expr_parser;
mod program_parser;
mod type_parser;
mod unop_expr_parser;

pub fn parse(mut tokens: TokenStack) -> ParsedProgram {
    let program = parse_program(&mut tokens).unwrap();
    program
}
