use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::item_id::{ItemId, ParsedFunctionId, ParsedTypeId};
use crate::compiler::parser::parsed_expression::{
    GenericParams, ParsedImport, ParsedType, ParsedTypeKind,
};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::primary_expr_parser::parse_generic_args;
use crate::compiler::parser::program_parser::parse_identifier;
use crate::compiler::parser::ModuleIdentifier;

pub fn parse_type(tokens: &mut TokenStack) -> ParseResult<ParsedType> {
    let token = tokens.pop().clone();
    let current_module = &token.location.file.as_ref().unwrap().id;
    match token.value {
        Token::Identifier(name) => {
            let parsed_type_id = parse_type_id(tokens, name, false, current_module)?;
            if parsed_type_id.is_module_local {
                match parsed_type_id.item_id.item_name.as_str() {
                    "unit" => return Ok(ParsedType::new(ParsedTypeKind::Unit, token.location)),
                    "bool" => return Ok(ParsedType::new(ParsedTypeKind::Bool, token.location)),
                    "char" => return Ok(ParsedType::new(ParsedTypeKind::Char, token.location)),
                    "byte" => {
                        return Ok(ParsedType::new(ParsedTypeKind::Integer(1), token.location))
                    }
                    "short" => {
                        return Ok(ParsedType::new(ParsedTypeKind::Integer(2), token.location))
                    }
                    "int" => {
                        return Ok(ParsedType::new(ParsedTypeKind::Integer(4), token.location))
                    }
                    "long" => {
                        return Ok(ParsedType::new(ParsedTypeKind::Integer(8), token.location))
                    }
                    _ => {}
                }
            }
            let generic_args = parse_generic_args(tokens)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Struct(parsed_type_id, generic_args),
                token.location,
            ))
        }
        Token::Static(StaticToken::DoubleColon) => {
            let first_id = parse_identifier(tokens)?;
            let parsed_type_id = parse_type_id(tokens, first_id.value, false, current_module)?;
            let generic_args = parse_generic_args(tokens)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Struct(parsed_type_id, generic_args),
                token.location,
            ))
        }
        Token::Static(StaticToken::Ampersand) => {
            let inner = parse_type(tokens)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Pointer(Box::new(inner.value)),
                token.location,
            ))
        }
        _ => Err(LocationError::new(
            format!("Expected type, found '{}'", token.value),
            token.location,
        ))?,
    }
}

pub fn parse_type_id(
    tokens: &mut TokenStack,
    name: String,
    is_absolute: bool,
    current_module: &ModuleIdentifier,
) -> ParseResult<ParsedTypeId> {
    let mut path = Vec::new();
    let mut cur_name = name;
    while tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        tokens.pop();
        path.push(cur_name.clone());
        let next_token = parse_identifier(tokens)?;
        cur_name = next_token.value;
    }

    let is_module_local = path.is_empty() && !is_absolute;
    let module_id = current_module.resolve(&path, is_absolute);
    Ok(ParsedTypeId {
        item_id: ItemId {
            module_id,
            item_name: cur_name,
        },
        is_module_local,
    })
}

pub fn parse_function_id(
    tokens: &mut TokenStack,
    name: String,
    is_absolute: bool,
    current_module: &ModuleIdentifier,
) -> ParseResult<(ParsedFunctionId, Vec<ParsedType>)> {
    let (parsed_type_id, generic_args) =
        parse_generic_type_id(tokens, name, is_absolute, current_module)?;

    Ok((
        ParsedFunctionId {
            item_id: parsed_type_id.item_id,
            is_module_local: parsed_type_id.is_module_local,
        },
        generic_args,
    ))
}

pub fn parse_import_id(
    tokens: &mut TokenStack,
    name: String,
    is_absolute: bool,
    current_module: &ModuleIdentifier,
) -> ParseResult<ParsedImport> {
    let mut path = Vec::new();
    let mut cur_name = Some(name);
    while tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        tokens.pop();
        path.push(cur_name.unwrap().clone());
        if tokens.peek().value == Token::Static(StaticToken::Asterisk) {
            tokens.pop();
            cur_name = None;
            break;
        }
        let next_token = parse_identifier(tokens)?;
        cur_name = Some(next_token.value);
    }

    let module_id = current_module.resolve(&path, is_absolute);

    Ok(ParsedImport {
        imported_object: cur_name,
        module_id,
    })
}

pub fn parse_generic_type_id(
    tokens: &mut TokenStack,
    name: String,
    is_absolute: bool,
    current_module: &ModuleIdentifier,
) -> ParseResult<(ParsedTypeId, Vec<ParsedType>)> {
    let mut path = Vec::new();
    let mut cur_name = name;
    let mut generic_args = Vec::new();
    while tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        tokens.pop();
        if tokens.peek().value == Token::Static(StaticToken::LessThan) {
            generic_args = parse_generic_args(tokens)?;
            break;
        } else {
            path.push(cur_name.clone());
            let next_token = parse_identifier(tokens)?;
            cur_name = next_token.value;
        }
    }

    let is_module_local = path.is_empty() && !is_absolute;
    let module_id = current_module.resolve(&path, is_absolute);
    Ok((
        ParsedTypeId {
            item_id: ItemId {
                module_id,
                item_name: cur_name,
            },
            is_module_local,
        },
        generic_args,
    ))
}
