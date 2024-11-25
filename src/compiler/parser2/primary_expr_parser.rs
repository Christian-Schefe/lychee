use anyhow::Context;
use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::token::{Keyword, Literal, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser2::parsed_expression::{ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryOp};
use crate::compiler::parser2::parser_error::ParseResult;
use crate::compiler::parser2::program_parser::{parse_expression, parse_identifier, pop_expected};
use crate::compiler::parser2::type_parser::parse_type;

pub fn parse_primary_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let token = tokens.peek().clone();
    match token.value {
        Token::Identifier(name) => {
            tokens.pop();
            match tokens.peek().value {
                Token::Static(StaticToken::OpenParen) => {
                    tokens.pop();
                    let mut args = Vec::new();
                    while tokens.peek().value != Token::Static(StaticToken::CloseParen) {
                        let arg_location = tokens.location().clone();
                        let arg = parse_expression(tokens).with_context(|| format!("Failed to parse function argument at {}.", arg_location))?;
                        args.push(arg);
                        match tokens.peek().value {
                            Token::Static(StaticToken::Comma) => {
                                tokens.pop();
                            }
                            Token::Static(StaticToken::CloseParen) => {}
                            _ => Err(LocationError::new("Expected comma or close paren after function argument.".to_string(), tokens.location().clone()))?,
                        }
                    }
                    pop_expected(tokens, Token::Static(StaticToken::CloseParen))?;
                    Ok(ParsedExpression::new(ParsedExpressionKind::FunctionCall { function_name: name, args }, token.location))
                }
                _ => {
                    Ok(ParsedExpression::new(ParsedExpressionKind::Variable(name), token.location))
                }
            }
        }
        Token::Literal(lit) => {
            tokens.pop();
            match lit {
                Literal::Integer(i) => Ok(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::Integer(i)), token.location)),
                Literal::Bool(f) => Ok(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::Bool(f)), token.location)),
                Literal::String(s) => Ok(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::String(s)), token.location)),
                Literal::Char(c) => Ok(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::Char(c as i8)), token.location)),
            }
        }
        Token::Static(StaticToken::OpenParen) => {
            tokens.pop();
            if let Token::Static(StaticToken::CloseParen) = tokens.peek().value {
                tokens.pop();
                return Ok(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::Unit), token.location));
            }
            let inner_location = tokens.location().clone();
            let mut inner = parse_expression(tokens).with_context(|| format!("Failed to parse parenthesized expression at {}.", inner_location))?;
            pop_expected(tokens, Token::Static(StaticToken::CloseParen))?;
            inner.location = token.location;
            Ok(inner)
        }
        Token::Static(StaticToken::OpenBrace) => {
            parse_block_expression(tokens).with_context(|| format!("Failed to parse block expression at {}.", token.location))
        }
        Token::Keyword(Keyword::Return) => {
            tokens.pop();
            match tokens.peek().value {
                Token::Static(StaticToken::Semicolon) | Token::Static(StaticToken::CloseBrace) => {
                    Ok(ParsedExpression::new(ParsedExpressionKind::Return(None), token.location))
                }
                _ => {
                    let expr_location = tokens.location().clone();
                    let expr = parse_expression(tokens).with_context(|| format!("Failed to parse return expression at {}.", expr_location))?;
                    Ok(ParsedExpression::new(ParsedExpressionKind::Return(Some(Box::new(expr))), token.location))
                }
            }
        }
        Token::Keyword(Keyword::Continue) => {
            tokens.pop();
            Ok(ParsedExpression::new(ParsedExpressionKind::Continue, token.location))
        }
        Token::Keyword(Keyword::Break) => {
            tokens.pop();
            match tokens.peek().value {
                Token::Static(StaticToken::Semicolon) | Token::Static(StaticToken::CloseBrace) => {
                    Ok(ParsedExpression::new(ParsedExpressionKind::Break(None), token.location))
                }
                _ => {
                    let expr_location = tokens.location().clone();
                    let expr = parse_expression(tokens).with_context(|| format!("Failed to parse break expression at {}.", expr_location))?;
                    Ok(ParsedExpression::new(ParsedExpressionKind::Break(Some(Box::new(expr))), token.location))
                }
            }
        }
        Token::Keyword(Keyword::If) => {
            tokens.pop();
            let condition_location = tokens.location().clone();
            let condition = Box::new(parse_expression(tokens).with_context(|| format!("Failed to parse if condition at {}.", condition_location))?);
            let then_location = tokens.location().clone();
            let then_block = Box::new(parse_block_expression(tokens).with_context(|| format!("Failed to parse then block at {}.", then_location))?);
            let else_block = if let Token::Keyword(Keyword::Else) = tokens.peek().value {
                tokens.pop();
                let else_location = tokens.location().clone();
                Some(Box::new(parse_expression(tokens).with_context(|| format!("Failed to parse else block at {}.", else_location))?))
            } else {
                None
            };
            Ok(ParsedExpression::new(ParsedExpressionKind::If { condition, then_block, else_expr: else_block }, token.location))
        }
        Token::Keyword(Keyword::While) => {
            tokens.pop();
            let condition_location = tokens.location().clone();
            let condition = Box::new(parse_expression(tokens).with_context(|| format!("Failed to parse while condition at {}.", condition_location))?);
            let loop_body_location = tokens.location().clone();
            let loop_body = Box::new(parse_block_expression(tokens).with_context(|| format!("Failed to parse loop body at {}.", loop_body_location))?);
            let false_block = if let Token::Keyword(Keyword::Else) = tokens.peek().value {
                tokens.pop();
                let else_location = tokens.location().clone();
                Some(Box::new(parse_expression(tokens).with_context(|| format!("Failed to parse else block at {}.", else_location))?))
            } else {
                None
            };
            Ok(ParsedExpression::new(ParsedExpressionKind::While { condition, loop_body, else_expr: false_block }, token.location))
        }
        Token::Keyword(Keyword::New) => {
            tokens.pop();
            let type_location = tokens.location().clone();
            let struct_type = parse_type(tokens).with_context(|| format!("Failed to parse struct type at {}.", type_location))?;
            pop_expected(tokens, Token::Static(StaticToken::OpenBrace))?;
            let mut fields = Vec::new();
            while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
                let field_name = parse_identifier(tokens)?.value;
                pop_expected(tokens, Token::Static(StaticToken::Colon))?;
                let expr_location = tokens.location().clone();
                let field_value = parse_expression(tokens).with_context(|| format!("Failed to parse struct field value at {}.", expr_location))?;
                fields.push((field_name, field_value));
                match tokens.peek().value {
                    Token::Static(StaticToken::Comma) => {
                        tokens.pop();
                    }
                    Token::Static(StaticToken::CloseBrace) => {}
                    _ => Err(LocationError::new("Expected comma or close brace after struct field value.".to_string(), tokens.location().clone()))?,
                }
            }
            pop_expected(tokens, Token::Static(StaticToken::CloseBrace))?;
            Ok(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::Struct(struct_type, fields)), token.location))
        }
        Token::Keyword(Keyword::Let) => {
            tokens.pop();
            let var_type = parse_type(tokens).with_context(|| format!("Failed to parse variable type at {}.", token.location))?;
            let var_name = parse_identifier(tokens)?.value;
            pop_expected(tokens, Token::Static(StaticToken::Assign))?;
            let value_location = tokens.location().clone();
            let value = parse_expression(tokens).with_context(|| format!("Failed to parse variable value at {}.", value_location))?;
            Ok(ParsedExpression::new(ParsedExpressionKind::Declaration { var_type, var_name, value: Box::new(value) }, token.location))
        }
        _ => Err(LocationError::new(format!("Expected primary expression, found '{}'", token.value), token.location))?,
    }
}

pub fn parse_block_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let token = pop_expected(tokens, Token::Static(StaticToken::OpenBrace))?;
    let mut expressions = Vec::new();
    let mut last_expr_has_semicolon = true;
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let expr_location = tokens.location().clone();
        let expr = parse_expression(tokens).with_context(|| format!("Failed to parse expression in block at {}.", expr_location))?;
        expressions.push(expr);
        match tokens.peek().value {
            Token::Static(StaticToken::Semicolon) => {
                tokens.pop();
                continue;
            }
            Token::Static(StaticToken::CloseBrace) => {
                last_expr_has_semicolon = false;
                break;
            }
            _ => Err(LocationError::new("Expected semicolon or close brace after expression in block".to_string(), tokens.location().clone()))?,
        }
    }
    if last_expr_has_semicolon {
        expressions.push(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::Unit), tokens.location().clone()));
    }
    pop_expected(tokens, Token::Static(StaticToken::CloseBrace))?;
    Ok(ParsedExpression::new(ParsedExpressionKind::Block(expressions), token.location))
}
