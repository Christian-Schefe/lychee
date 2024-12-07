use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::merger::merged_expression::ModuleId;
use crate::compiler::parser::parsed_expression::{ParsedType, ParsedTypeKind};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::program_parser::parse_identifier;
use crate::compiler::parser::ModuleIdentifier;

pub fn parse_type(tokens: &mut TokenStack) -> ParseResult<ParsedType> {
    let token = tokens.pop().clone();
    match token.value {
        Token::Identifier(name) => {
            let module_id = parse_module_path(tokens, name, false)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Named(module_id),
                token.location,
            ))
        }
        Token::Static(StaticToken::DoubleColon) => {
            let first_id = parse_identifier(tokens)?;
            let module_id = parse_module_path(tokens, first_id.value, true)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Named(module_id),
                token.location,
            ))
        }
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

pub fn parse_module_path(
    tokens: &mut TokenStack,
    name: String,
    is_absolute: bool,
) -> ParseResult<ModuleId> {
    let mut path = Vec::new();
    let mut cur_name = name;
    while tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        tokens.pop();
        path.push(cur_name.clone());
        let next_token = parse_identifier(tokens)?;
        cur_name = next_token.value;
    }
    let module_id = ModuleId {
        module_path: ModuleIdentifier {
            path,
            absolute: is_absolute,
        },
        name: cur_name,
    };
    Ok(module_id)
}
