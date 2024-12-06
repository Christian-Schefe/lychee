use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::merger::merged_expression::ModuleId;
use crate::compiler::parser::parsed_expression::{ParsedType, ParsedTypeKind};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::ModulePath;

pub fn parse_type(tokens: &mut TokenStack) -> ParseResult<ParsedType> {
    let token = tokens.pop().clone();
    match token.value {
        Token::Identifier(name) => {
            let module_id = parse_module_path(tokens, name)?;
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

pub fn parse_module_path(tokens: &mut TokenStack, name: String) -> ParseResult<ModuleId> {
    let mut path = Vec::new();
    let mut cur_name = name;
    while tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        tokens.pop();
        path.push(cur_name.clone());
        let next_token = tokens.pop().clone();
        match next_token.value {
            Token::Identifier(next_name) => cur_name = next_name,
            _ => Err(LocationError::new(
                format!("Expected identifier, found '{}'", next_token.value),
                next_token.location,
            ))?,
        }
    }
    let module_id = ModuleId {
        module_path: ModulePath(path),
        name: cur_name,
    };
    Ok(module_id)
}
