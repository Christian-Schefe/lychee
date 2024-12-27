use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::item_id::{ItemId, ParsedScopeId};
use crate::compiler::parser::parsed_expression::{ParsedImport, ParsedType, ParsedTypeKind};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::parsing_utils::parse_seperated_elements;
use crate::compiler::parser::primary_expr_parser::parse_generic_args;
use crate::compiler::parser::program_parser::parse_identifier;
use crate::compiler::parser::ModuleIdentifier;

pub fn parse_type(tokens: &mut TokenStack) -> ParseResult<ParsedType> {
    let token = tokens.peek();
    let current_module = token.location.file.as_ref().unwrap().id.clone();
    let location = token.location.clone();
    match &token.value {
        Token::Identifier(_) | Token::Static(StaticToken::DoubleColon) => {
            let parsed_type_id = parse_scoped_id(tokens, &current_module)?;
            if parsed_type_id.is_module_local {
                let builtin_kind = match parsed_type_id.item_id.item_name.as_str() {
                    "unit" => Some(ParsedTypeKind::Unit),
                    "bool" => Some(ParsedTypeKind::Bool),
                    "char" => Some(ParsedTypeKind::Char),
                    "byte" => Some(ParsedTypeKind::Integer(1)),
                    "short" => Some(ParsedTypeKind::Integer(2)),
                    "int" => Some(ParsedTypeKind::Integer(4)),
                    "long" => Some(ParsedTypeKind::Integer(8)),
                    _ => None,
                };
                if let Some(kind) = builtin_kind {
                    return Ok(ParsedType::new(kind, location));
                }
            }
            let generic_args = parse_generic_args(tokens)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Struct(parsed_type_id, generic_args),
                location,
            ))
        }
        Token::Static(StaticToken::Ampersand) => {
            tokens.shift();
            let inner = parse_type(tokens)?;
            Ok(ParsedType::new(
                ParsedTypeKind::Pointer(Box::new(inner.value)),
                location,
            ))
        }
        Token::Static(StaticToken::OpenParen) => {
            let (loc, elements, _) = parse_seperated_elements(
                tokens,
                Token::Static(StaticToken::OpenParen),
                Token::Static(StaticToken::CloseParen),
                Token::Static(StaticToken::Comma),
                false,
                true,
                "function type",
                parse_type,
            )?;

            if tokens.peek().value == Token::Static(StaticToken::Arrow) {
                tokens.shift();
                let return_type = parse_type(tokens)?;
                Ok(ParsedType::new(
                    ParsedTypeKind::Function {
                        return_type: Box::new(return_type),
                        params: elements,
                    },
                    loc,
                ))
            } else {
                if elements.len() == 0 {
                    Err(anyhow::anyhow!(
                        "Expected at least one element in tuple type at {}",
                        loc
                    ))
                } else {
                    Ok(ParsedType::new(
                        ParsedTypeKind::Struct(
                            ParsedScopeId {
                                item_id: ItemId {
                                    module_id: current_module,
                                    item_name: "$tuple".to_string(),
                                },
                                is_module_local: true,
                            },
                            elements,
                        ),
                        loc,
                    ))
                }
            }
        }
        _ => Err(LocationError::new(
            format!("Expected type, found '{}'", token.value),
            location,
        ))?,
    }
}

pub fn parse_import_id(
    tokens: &mut TokenStack,
    name: String,
    is_absolute: bool,
    current_module: &ModuleIdentifier,
) -> ParseResult<ParsedImport> {
    let mut path = Vec::new();
    let mut cur_name = Some(name);
    let mut imported_object_vec = None;

    while tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        tokens.shift();
        path.push(cur_name.unwrap().clone());
        if tokens.peek().value == Token::Static(StaticToken::Asterisk) {
            tokens.shift();
            imported_object_vec = None;
            break;
        }
        if tokens.peek().value == Token::Static(StaticToken::OpenBrace) {
            tokens.shift();
            let mut imported_objects = Vec::new();
            while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
                let next_token = parse_identifier(tokens)?;
                imported_objects.push(next_token.value);
                match tokens.peek().value {
                    Token::Static(StaticToken::Comma) => {
                        tokens.shift();
                    }
                    Token::Static(StaticToken::CloseBrace) => {}
                    _ => Err(LocationError::new(
                        format!("Expected ',' or '}}', found '{}'", tokens.peek().value),
                        tokens.location().clone(),
                    ))?,
                }
            }
            tokens.shift();
            imported_object_vec = Some(imported_objects);
            break;
        }
        let next_token = parse_identifier(tokens)?;
        cur_name = Some(next_token.value.clone());
        if tokens.peek().value != Token::Static(StaticToken::DoubleColon) {
            imported_object_vec = Some(vec![next_token.value]);
            break;
        }
    }

    let module_id = current_module.resolve(&path, is_absolute);

    Ok(ParsedImport {
        imported_objects: imported_object_vec,
        module_id,
    })
}

pub fn parse_generic_scoped_id_extension(
    tokens: &mut TokenStack,
) -> ParseResult<Option<Vec<ParsedType>>> {
    let generic_args = if tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        tokens.shift();
        if tokens.peek().value == Token::Static(StaticToken::LessThan) {
            Some(parse_generic_args(tokens)?)
        } else {
            tokens.reverse_shift();
            None
        }
    } else {
        None
    };

    Ok(generic_args)
}

pub fn parse_scoped_id(
    tokens: &mut TokenStack,
    current_module: &ModuleIdentifier,
) -> ParseResult<ParsedScopeId> {
    let is_relative = tokens.peek().value == Token::Static(StaticToken::DoubleColon);
    if is_relative {
        tokens.shift();
    }
    let first_token = parse_identifier(tokens)?;
    let mut path = vec![first_token.value];
    while tokens.peek().value == Token::Static(StaticToken::DoubleColon) {
        let old_offset = tokens.offset;
        tokens.shift();
        let next_token = parse_identifier(tokens);
        if let Ok(next_token) = next_token {
            path.push(next_token.value);
        } else {
            tokens.offset = old_offset;
            break;
        }
    }
    
    let is_relative = is_relative || path.len() == 1;

    let item_name = path.pop().unwrap();
    let is_module_local = path.is_empty() && is_relative;
    let module_id = current_module.resolve(&path, !is_relative);
    let id = ParsedScopeId {
        item_id: ItemId {
            module_id,
            item_name,
        },
        is_module_local,
    };
    Ok(id)
}
