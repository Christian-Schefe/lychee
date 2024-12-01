use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parsed_expression::{ParsedType, ParsedTypeKind};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::program_parser::pop_expected;

pub fn parse_type(tokens: &mut TokenStack) -> ParseResult<ParsedType> {
    let token = tokens.pop().clone();
    match token.value {
        Token::Identifier(name) => Ok(ParsedType::new(ParsedTypeKind::Named(name), token.location)),
        Token::Static(StaticToken::OpenBracket) => {
            let inner = parse_type(tokens)?;
            pop_expected(tokens, Token::Static(StaticToken::CloseBracket))?;
            Ok(ParsedType::new(ParsedTypeKind::Array(Box::new(inner)), token.location))
        }
        Token::Static(StaticToken::Ampersand) => {
            let inner = parse_type(tokens)?;
            Ok(ParsedType::new(ParsedTypeKind::Pointer(Box::new(inner)), token.location))
        }
        _ => Err(LocationError::new(format!("Expected type, found '{}'", token.value), token.location))?,
    }
}