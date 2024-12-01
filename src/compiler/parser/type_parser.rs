use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parsed_expression::{ParsedType, ParsedTypeKind};
use crate::compiler::parser::parser_error::ParseResult;

pub fn parse_type(tokens: &mut TokenStack) -> ParseResult<ParsedType> {
    let token = tokens.pop().clone();
    match token.value {
        Token::Identifier(name) => Ok(ParsedType::new(ParsedTypeKind::Named(name), token.location)),
        Token::Static(StaticToken::Ampersand) => {
            let inner = parse_type(tokens)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Pointer(Box::new(inner)),
                token.location,
            ))
        }
        _ => Err(LocationError::new(
            format!("Expected type, found '{}'", token.value),
            token.location,
        ))?,
    }
}
