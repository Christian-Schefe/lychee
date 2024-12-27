use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::location::Location;
use crate::compiler::lexer::token::{Keyword, Literal, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::lexer::SrcToken;
use crate::compiler::parser::item_id::ParsedGenericId;
use crate::compiler::parser::parsed_expression::{
    ParsedExpression, ParsedExpressionKind, ParsedGenericParams, ParsedLiteral, ParsedType,
    ParsedTypeKind,
};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::parsing_utils::parse_seperated_elements;
use crate::compiler::parser::program_parser::{parse_expression, parse_identifier, pop_expected};
use crate::compiler::parser::type_parser::{
    parse_generic_scoped_id_extension, parse_scoped_id, parse_type,
};
use anyhow::Context;
use std::collections::HashSet;

pub fn parse_primary_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let token = tokens.peek().clone();
    let current_module = &token.location.file.as_ref().unwrap().id;
    match token.value {
        Token::Identifier(_) | Token::Static(StaticToken::DoubleColon) => {
            let id = parse_scoped_id(tokens, current_module)
                .with_context(|| format!("Failed to parse identifier at {}.", token.location))?;
            let generic_args = parse_generic_scoped_id_extension(tokens).with_context(|| {
                format!("Failed to parse generic arguments at {}.", token.location)
            })?;
            let generic_id = ParsedGenericId {
                id: id,
                generic_args,
            };
            let var_expr = ParsedExpression::new(
                ParsedExpressionKind::Variable(generic_id),
                token.location.clone(),
            );

            Ok(var_expr)
        }
        Token::Literal(lit) => {
            tokens.shift();
            match lit {
                Literal::Integer(i) => Ok(ParsedExpression::new(
                    ParsedExpressionKind::Literal(ParsedLiteral::Integer(i)),
                    token.location,
                )),
                Literal::Bool(f) => Ok(ParsedExpression::new(
                    ParsedExpressionKind::Literal(ParsedLiteral::Bool(f)),
                    token.location,
                )),
                Literal::String(s) => Ok(ParsedExpression::new(
                    ParsedExpressionKind::Literal(ParsedLiteral::String(s)),
                    token.location,
                )),
                Literal::Char(c) => Ok(ParsedExpression::new(
                    ParsedExpressionKind::Literal(ParsedLiteral::Char(c as i8)),
                    token.location,
                )),
            }
        }
        Token::Static(StaticToken::OpenParen) => {
            let (loc, elements, is_trailing) = parse_seperated_elements(
                tokens,
                Token::Static(StaticToken::OpenParen),
                Token::Static(StaticToken::CloseParen),
                Token::Static(StaticToken::Comma),
                true,
                true,
                "tuple",
                parse_expression,
            )?;
            if elements.len() == 0 {
                Ok(ParsedExpression::new(
                    ParsedExpressionKind::Literal(ParsedLiteral::Unit),
                    loc,
                ))
            } else if elements.len() == 1 && !is_trailing {
                let inner = elements.into_iter().next().unwrap();
                Ok(inner)
            } else {
                Ok(ParsedExpression::new(
                    ParsedExpressionKind::Tuple(elements),
                    loc,
                ))
            }
        }
        Token::Static(StaticToken::OpenBrace) => parse_block_expression(tokens)
            .with_context(|| format!("Failed to parse block expression at {}.", token.location)),
        Token::Keyword(Keyword::Return) => {
            tokens.shift();
            let expr = parse_expression(tokens).map(|x| Box::new(x)).ok();
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Return(expr),
                token.location,
            ))
        }
        Token::Keyword(Keyword::Continue) => {
            tokens.shift();
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Continue,
                token.location,
            ))
        }
        Token::Keyword(Keyword::Break) => {
            tokens.shift();
            let expr = parse_expression(tokens).map(|x| Box::new(x)).ok();
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Break(expr),
                token.location,
            ))
        }
        Token::Keyword(Keyword::If) => {
            tokens.shift();
            let condition_location = tokens.location().clone();
            let condition = Box::new(parse_expression(tokens).with_context(|| {
                format!("Failed to parse if condition at {}.", condition_location)
            })?);
            let then_location = tokens.location().clone();
            let then_block =
                Box::new(parse_block_expression(tokens).with_context(|| {
                    format!("Failed to parse then block at {}.", then_location)
                })?);
            let else_block = parse_optional_else_block(tokens)?;
            Ok(ParsedExpression::new(
                ParsedExpressionKind::If {
                    condition,
                    then_block,
                    else_expr: else_block.map(|x| Box::new(x)),
                },
                token.location,
            ))
        }
        Token::Keyword(Keyword::While) => {
            tokens.shift();
            let condition_location = tokens.location().clone();
            let condition = Box::new(parse_expression(tokens).with_context(|| {
                format!("Failed to parse while condition at {}.", condition_location)
            })?);
            let loop_body_location = tokens.location().clone();
            let loop_body = Box::new(parse_block_expression(tokens).with_context(|| {
                format!("Failed to parse loop body at {}.", loop_body_location)
            })?);
            let false_block = parse_optional_else_block(tokens)?;
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Loop {
                    init: None,
                    condition: Some(condition),
                    step: None,
                    loop_body,
                    else_expr: false_block.map(|x| Box::new(x)),
                },
                token.location,
            ))
        }
        Token::Keyword(Keyword::For) => {
            tokens.shift();
            pop_expected(tokens, Token::Static(StaticToken::OpenParen))?;
            let init_location = tokens.location().clone();
            let init = Box::new(
                parse_expression(tokens)
                    .with_context(|| format!("Failed to parse for init at {}.", init_location))?,
            );
            pop_expected(tokens, Token::Static(StaticToken::Semicolon))?;
            let condition_location = tokens.location().clone();
            let condition = Box::new(parse_expression(tokens).with_context(|| {
                format!("Failed to parse for condition at {}.", condition_location)
            })?);
            pop_expected(tokens, Token::Static(StaticToken::Semicolon))?;
            let step_location = tokens.location().clone();
            let step = Box::new(
                parse_expression(tokens)
                    .with_context(|| format!("Failed to parse for step at {}.", step_location))?,
            );
            pop_expected(tokens, Token::Static(StaticToken::CloseParen))?;
            let loop_body_location = tokens.location().clone();
            let loop_body = Box::new(parse_block_expression(tokens).with_context(|| {
                format!("Failed to parse for loop body at {}.", loop_body_location)
            })?);
            let false_block = parse_optional_else_block(tokens)?;
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Loop {
                    init: Some(init),
                    condition: Some(condition),
                    step: Some(step),
                    loop_body,
                    else_expr: false_block.map(|x| Box::new(x)),
                },
                token.location,
            ))
        }
        Token::Keyword(Keyword::Loop) => {
            tokens.shift();
            let loop_body_location = tokens.location().clone();
            let loop_body = Box::new(parse_block_expression(tokens).with_context(|| {
                format!("Failed to parse loop body at {}.", loop_body_location)
            })?);
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Loop {
                    init: None,
                    condition: None,
                    step: None,
                    loop_body,
                    else_expr: None,
                },
                token.location,
            ))
        }
        Token::Keyword(Keyword::New) => {
            tokens.shift();
            let type_location = tokens.location().clone();
            let ty = parse_type(tokens)
                .with_context(|| format!("Failed to parse type at {}.", type_location))?;
            match &ty.value {
                ParsedTypeKind::Struct(_, _) => {
                    parse_struct_literal(tokens, ty, token.location.clone()).with_context(|| {
                        format!("Failed to parse struct literal at {}.", token.location)
                    })
                }
                _ => Err(LocationError::new(
                    "Expected named type for new expression.".to_string(),
                    type_location,
                ))?,
            }
        }
        Token::Keyword(Keyword::Let) => {
            tokens.shift();
            let var_type = match tokens.peek().value {
                Token::Keyword(Keyword::Var) => {
                    tokens.shift();
                    None
                }
                _ => Some(parse_type(tokens).with_context(|| {
                    format!("Failed to parse variable type at {}.", token.location)
                })?),
            };
            let var_name = parse_identifier(tokens)?.value;
            pop_expected(tokens, Token::Static(StaticToken::Assign))?;
            let value_location = tokens.location().clone();
            let value = parse_expression(tokens).with_context(|| {
                format!("Failed to parse variable value at {}.", value_location)
            })?;
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Declaration {
                    var_type,
                    var_name,
                    value: Box::new(value),
                },
                token.location,
            ))
        }
        Token::Keyword(Keyword::Sizeof) => {
            tokens.shift();
            pop_expected(tokens, Token::Static(StaticToken::OpenParen))?;
            let ty_location = tokens.location().clone();
            let ty = parse_type(tokens)
                .with_context(|| format!("Failed to parse type at {}.", ty_location))?;
            pop_expected(tokens, Token::Static(StaticToken::CloseParen))?;
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Sizeof(ty),
                token.location,
            ))
        }
        _ => Err(LocationError::new(
            format!("Expected primary expression, found '{}'", token.value),
            token.location,
        ))?,
    }
}

pub fn parse_block_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let (location, expressions, has_trailed) = parse_seperated_elements(
        tokens,
        Token::Static(StaticToken::OpenBrace),
        Token::Static(StaticToken::CloseBrace),
        Token::Static(StaticToken::Semicolon),
        true,
        true,
        "block",
        parse_expression,
    )?;
    Ok(ParsedExpression::new(
        ParsedExpressionKind::Block {
            expressions,
            returns_value: !has_trailed,
        },
        location,
    ))
}

fn parse_struct_literal(
    tokens: &mut TokenStack,
    struct_type: ParsedType,
    location: Location,
) -> ParseResult<ParsedExpression> {
    pop_expected(tokens, Token::Static(StaticToken::OpenBrace))?;
    let mut fields = Vec::new();
    let mut used_field_names = HashSet::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let field_name = parse_identifier(tokens)?.value;
        pop_expected(tokens, Token::Static(StaticToken::Colon))?;
        let expr_location = tokens.location().clone();
        let field_value = parse_expression(tokens)
            .with_context(|| format!("Failed to parse struct field value at {}.", expr_location))?;
        fields.push((field_name.clone(), field_value));
        if !used_field_names.insert(field_name.clone()) {
            Err(LocationError::new(
                format!("Duplicate field name '{}' in struct literal.", field_name),
                expr_location,
            ))?;
        }
        match tokens.peek().value {
            Token::Static(StaticToken::Comma) => {
                tokens.shift();
            }
            Token::Static(StaticToken::CloseBrace) => {}
            _ => Err(LocationError::new(
                "Expected comma or close brace after struct field value.".to_string(),
                tokens.location().clone(),
            ))?,
        }
    }
    pop_expected(tokens, Token::Static(StaticToken::CloseBrace))?;
    Ok(ParsedExpression::new(
        ParsedExpressionKind::StructInstance {
            struct_type,
            fields,
        },
        location,
    ))
}

fn parse_optional_else_block(tokens: &mut TokenStack) -> ParseResult<Option<ParsedExpression>> {
    if let Token::Keyword(Keyword::Else) = tokens.peek().value {
        tokens.shift();
        let else_location = tokens.location().clone();
        let else_block = parse_expression(tokens)
            .with_context(|| format!("Failed to parse else block at {}.", else_location))?;
        Ok(Some(else_block))
    } else {
        Ok(None)
    }
}

pub fn parse_generic_args(tokens: &mut TokenStack) -> ParseResult<Vec<ParsedType>> {
    if let Token::Static(StaticToken::LessThan) = tokens.peek().value {
        tokens.shift();
        let mut generics = Vec::new();
        while tokens.peek().value != Token::Static(StaticToken::GreaterThan) {
            let generic = parse_type(tokens)?;
            generics.push(generic);
            match tokens.peek().value {
                Token::Static(StaticToken::Comma) => {
                    tokens.shift();
                }
                Token::Static(StaticToken::GreaterThan) => {}
                Token::Static(StaticToken::ShiftRight) => {
                    //split the >> token into two >
                    tokens.replace(SrcToken {
                        value: Token::Static(StaticToken::GreaterThan),
                        location: tokens.location().clone(),
                    });
                    tokens.insert(SrcToken {
                        value: Token::Static(StaticToken::GreaterThan),
                        location: tokens.location().clone(),
                    });
                }
                _ => Err(anyhow::anyhow!(
                    "Expected , or > after generic type, found {} at {}",
                    tokens.peek().value,
                    tokens.location()
                ))?,
            }
        }
        pop_expected(tokens, Token::Static(StaticToken::GreaterThan))?;
        Ok(generics)
    } else {
        Ok(Vec::new())
    }
}

pub fn parse_generic_params(tokens: &mut TokenStack) -> ParseResult<ParsedGenericParams> {
    if let Token::Static(StaticToken::LessThan) = tokens.peek().value {
        tokens.shift();
        let mut generics = Vec::new();
        while tokens.peek().value != Token::Static(StaticToken::GreaterThan) {
            let generic = parse_identifier(tokens)?.value;
            generics.push(generic);
            match tokens.peek().value {
                Token::Static(StaticToken::Comma) => {
                    tokens.shift();
                }
                Token::Static(StaticToken::GreaterThan) => {}
                _ => Err(LocationError::new(
                    "Expected comma or greater than after generic.".to_string(),
                    tokens.location().clone(),
                ))?,
            }
        }
        pop_expected(tokens, Token::Static(StaticToken::GreaterThan))?;
        Ok(ParsedGenericParams::new(generics)?)
    } else {
        Ok(ParsedGenericParams::empty())
    }
}

pub fn parse_function_call(
    tokens: &mut TokenStack,
    function_expr: ParsedExpression,
    location: Location,
    member_call: Option<ParsedExpression>,
) -> ParseResult<ParsedExpression> {
    let (_, mut args, _) = parse_seperated_elements(
        tokens,
        Token::Static(StaticToken::OpenParen),
        Token::Static(StaticToken::CloseParen),
        Token::Static(StaticToken::Comma),
        false,
        true,
        "function call arguments",
        parse_expression,
    )?;
    if let Some(member_obj) = member_call {
        args.insert(0, member_obj);
    }
    Ok(ParsedExpression::new(
        ParsedExpressionKind::FunctionCall {
            expr: Box::new(function_expr),
            args,
        },
        location,
    ))
}
