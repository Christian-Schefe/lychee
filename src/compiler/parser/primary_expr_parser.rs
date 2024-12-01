use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::location::Location;
use crate::compiler::lexer::token::{Keyword, Literal, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parsed_expression::{
    ParsedExpression, ParsedExpressionKind, ParsedLiteral, ParsedType, ParsedTypeKind,
};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::program_parser::{parse_expression, parse_identifier, pop_expected};
use crate::compiler::parser::type_parser::parse_type;
use anyhow::Context;
use std::collections::HashMap;

pub fn parse_primary_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let token = tokens.peek().clone();
    match token.value {
        Token::Identifier(name) => {
            tokens.pop();
            match tokens.peek().value {
                Token::Static(StaticToken::OpenParen) => {
                    let (_, args, _) = parse_seperated_expressions(
                        tokens,
                        Token::Static(StaticToken::OpenParen),
                        Token::Static(StaticToken::CloseParen),
                        Token::Static(StaticToken::Comma),
                        false,
                        "function call arguments",
                    )?;
                    Ok(ParsedExpression::new(
                        ParsedExpressionKind::FunctionCall {
                            function_name: name,
                            args,
                        },
                        token.location,
                    ))
                }
                _ => Ok(ParsedExpression::new(
                    ParsedExpressionKind::Variable(name),
                    token.location,
                )),
            }
        }
        Token::Literal(lit) => {
            tokens.pop();
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
            tokens.pop();
            if let Token::Static(StaticToken::CloseParen) = tokens.peek().value {
                tokens.pop();
                return Ok(ParsedExpression::new(
                    ParsedExpressionKind::Literal(ParsedLiteral::Unit),
                    token.location,
                ));
            }
            let inner_location = tokens.location().clone();
            let mut inner = parse_expression(tokens).with_context(|| {
                format!(
                    "Failed to parse parenthesized expression at {}.",
                    inner_location
                )
            })?;
            pop_expected(tokens, Token::Static(StaticToken::CloseParen))?;
            inner.location = token.location;
            Ok(inner)
        }
        Token::Static(StaticToken::OpenBrace) => parse_block_expression(tokens)
            .with_context(|| format!("Failed to parse block expression at {}.", token.location)),
        Token::Keyword(Keyword::Return) => {
            tokens.pop();
            match tokens.peek().value {
                Token::Static(StaticToken::Semicolon) | Token::Static(StaticToken::CloseBrace) => {
                    Ok(ParsedExpression::new(
                        ParsedExpressionKind::Return(None),
                        token.location,
                    ))
                }
                _ => {
                    let expr_location = tokens.location().clone();
                    let expr = parse_expression(tokens).with_context(|| {
                        format!("Failed to parse return expression at {}.", expr_location)
                    })?;
                    Ok(ParsedExpression::new(
                        ParsedExpressionKind::Return(Some(Box::new(expr))),
                        token.location,
                    ))
                }
            }
        }
        Token::Keyword(Keyword::Continue) => {
            tokens.pop();
            Ok(ParsedExpression::new(
                ParsedExpressionKind::Continue,
                token.location,
            ))
        }
        Token::Keyword(Keyword::Break) => {
            tokens.pop();
            match tokens.peek().value {
                Token::Static(StaticToken::Semicolon) | Token::Static(StaticToken::CloseBrace) => {
                    Ok(ParsedExpression::new(
                        ParsedExpressionKind::Break(None),
                        token.location,
                    ))
                }
                _ => {
                    let expr_location = tokens.location().clone();
                    let expr = parse_expression(tokens).with_context(|| {
                        format!("Failed to parse break expression at {}.", expr_location)
                    })?;
                    Ok(ParsedExpression::new(
                        ParsedExpressionKind::Break(Some(Box::new(expr))),
                        token.location,
                    ))
                }
            }
        }
        Token::Keyword(Keyword::If) => {
            tokens.pop();
            let condition_location = tokens.location().clone();
            let condition = Box::new(parse_expression(tokens).with_context(|| {
                format!("Failed to parse if condition at {}.", condition_location)
            })?);
            let then_location = tokens.location().clone();
            let then_block =
                Box::new(parse_block_expression(tokens).with_context(|| {
                    format!("Failed to parse then block at {}.", then_location)
                })?);
            let else_block = if let Token::Keyword(Keyword::Else) = tokens.peek().value {
                tokens.pop();
                let else_location = tokens.location().clone();
                Some(Box::new(parse_expression(tokens).with_context(|| {
                    format!("Failed to parse else block at {}.", else_location)
                })?))
            } else {
                None
            };
            Ok(ParsedExpression::new(
                ParsedExpressionKind::If {
                    condition,
                    then_block,
                    else_expr: else_block,
                },
                token.location,
            ))
        }
        Token::Keyword(Keyword::While) => {
            tokens.pop();
            let condition_location = tokens.location().clone();
            let condition = Box::new(parse_expression(tokens).with_context(|| {
                format!("Failed to parse while condition at {}.", condition_location)
            })?);
            let loop_body_location = tokens.location().clone();
            let loop_body = Box::new(parse_block_expression(tokens).with_context(|| {
                format!("Failed to parse loop body at {}.", loop_body_location)
            })?);
            let false_block = if let Token::Keyword(Keyword::Else) = tokens.peek().value {
                tokens.pop();
                let else_location = tokens.location().clone();
                Some(Box::new(parse_expression(tokens).with_context(|| {
                    format!("Failed to parse else block at {}.", else_location)
                })?))
            } else {
                None
            };
            Ok(ParsedExpression::new(
                ParsedExpressionKind::While {
                    condition,
                    loop_body,
                    else_expr: false_block,
                },
                token.location,
            ))
        }
        Token::Keyword(Keyword::New) => {
            tokens.pop();
            let type_location = tokens.location().clone();
            let ty = parse_type(tokens)
                .with_context(|| format!("Failed to parse type at {}.", type_location))?;
            match &ty.value {
                ParsedTypeKind::Named(_) => {
                    parse_struct_literal(tokens, ty, token.location.clone()).with_context(|| {
                        format!("Failed to parse struct literal at {}.", token.location)
                    })
                }
                ParsedTypeKind::Array(_) => {
                    let (_, expressions, _) = parse_seperated_expressions(
                        tokens,
                        Token::Static(StaticToken::OpenBrace),
                        Token::Static(StaticToken::CloseBrace),
                        Token::Static(StaticToken::Comma),
                        false,
                        "array",
                    )?;
                    Ok(ParsedExpression::new(
                        ParsedExpressionKind::Literal(ParsedLiteral::Array(ty, expressions)),
                        token.location,
                    ))
                }
                _ => Err(LocationError::new(
                    "Expected named type or array type for new expression.".to_string(),
                    type_location,
                ))?,
            }
        }
        Token::Keyword(Keyword::Let) => {
            tokens.pop();
            let var_type = match tokens.peek().value {
                Token::Keyword(Keyword::Var) => {
                    tokens.pop();
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
        _ => Err(LocationError::new(
            format!("Expected primary expression, found '{}'", token.value),
            token.location,
        ))?,
    }
}

pub fn parse_block_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let (location, expressions, has_trailed) = parse_seperated_expressions(
        tokens,
        Token::Static(StaticToken::OpenBrace),
        Token::Static(StaticToken::CloseBrace),
        Token::Static(StaticToken::Semicolon),
        true,
        "block",
    )?;
    Ok(ParsedExpression::new(
        ParsedExpressionKind::Block {
            expressions,
            returns_value: !has_trailed,
        },
        location,
    ))
}

pub fn parse_seperated_expressions(
    tokens: &mut TokenStack,
    open_token: Token,
    close_token: Token,
    separator_token: Token,
    allow_trailing: bool,
    component_name: &str,
) -> ParseResult<(Location, Vec<ParsedExpression>, bool)> {
    let token = pop_expected(tokens, open_token)?;
    let mut expressions = Vec::new();
    let mut has_trailed = false;
    while tokens.peek().value != close_token {
        let expr_location = tokens.location().clone();
        let expr = parse_expression(tokens).with_context(|| {
            format!(
                "Failed to parse expression in {component_name} at {}.",
                expr_location
            )
        })?;
        expressions.push(expr);
        if tokens.peek().value == close_token {
            break;
        } else if tokens.peek().value == separator_token {
            tokens.pop();
            if tokens.peek().value == close_token {
                has_trailed = true;
                break;
            }
        } else {
            Err(anyhow::anyhow!(
                "Expected {} or {} after expression in {component_name} at {}.",
                separator_token,
                close_token,
                tokens.location().clone()
            ))?;
        }
    }
    if !allow_trailing && has_trailed {
        Err(anyhow::anyhow!(
            "Unexpected trailing {} in {component_name} at {}.",
            separator_token,
            tokens.location().clone()
        ))?;
    }
    pop_expected(tokens, close_token)?;
    Ok((token.location, expressions, has_trailed))
}

fn parse_struct_literal(
    tokens: &mut TokenStack,
    struct_type: ParsedType,
    location: Location,
) -> ParseResult<ParsedExpression> {
    pop_expected(tokens, Token::Static(StaticToken::OpenBrace))?;
    let mut fields = HashMap::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let field_name = parse_identifier(tokens)?.value;
        pop_expected(tokens, Token::Static(StaticToken::Colon))?;
        let expr_location = tokens.location().clone();
        let field_value = parse_expression(tokens)
            .with_context(|| format!("Failed to parse struct field value at {}.", expr_location))?;
        if fields.insert(field_name.clone(), field_value).is_some() {
            Err(LocationError::new(
                format!("Duplicate field name '{}'", field_name),
                expr_location,
            ))?;
        }
        match tokens.peek().value {
            Token::Static(StaticToken::Comma) => {
                tokens.pop();
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
        ParsedExpressionKind::Literal(ParsedLiteral::Struct(struct_type, fields)),
        location,
    ))
}
