use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::binary_op::BinaryOp;
use crate::compiler::parser::item_id::ParsedGenericId;
use crate::compiler::parser::parsed_expression::{
    ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryMathOp, UnaryOp,
};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::primary_expr_parser::{parse_function_call, parse_primary_expression};
use crate::compiler::parser::program_parser::{parse_expression, pop_expected};
use crate::compiler::parser::type_parser::{parse_generic_scoped_id_extension, parse_scoped_id};
use anyhow::Context;

fn parse_prefix_unary<F>(
    tokens: &mut TokenStack,
    op_tokens: &[(StaticToken, UnaryOp)],
    parse_lower: F,
) -> ParseResult<ParsedExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<ParsedExpression>,
{
    let token = tokens.peek().clone();
    if let Some((_, op)) = op_tokens.iter().find(|t| {
        if let Token::Static(tkn) = &token.value {
            *tkn == t.0
        } else {
            false
        }
    }) {
        tokens.shift();
        let inner = parse_prefix_unary(tokens, op_tokens, parse_lower)?;
        Ok(ParsedExpression::new(
            ParsedExpressionKind::Unary {
                expr: Box::new(inner),
                op: op.clone(),
            },
            token.location,
        ))
    } else {
        parse_lower(tokens)
    }
}

pub fn parse_unop_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let expr = parse_prefix_unary(
        tokens,
        &[
            (StaticToken::Ampersand, UnaryOp::Borrow),
            (StaticToken::Asterisk, UnaryOp::Dereference),
            (StaticToken::Minus, UnaryOp::Math(UnaryMathOp::Negate)),
            (StaticToken::Plus, UnaryOp::Math(UnaryMathOp::Positive)),
            (StaticToken::Tilde, UnaryOp::Math(UnaryMathOp::BitwiseNot)),
            (StaticToken::ExclamationMark, UnaryOp::LogicalNot),
            (
                StaticToken::Increment,
                UnaryOp::Increment { is_prefix: true },
            ),
            (
                StaticToken::Decrement,
                UnaryOp::Decrement { is_prefix: true },
            ),
        ],
        parse_postfix_unary,
    )?;

    match &expr.value {
        ParsedExpressionKind::Unary {
            op: UnaryOp::Math(UnaryMathOp::Negate),
            expr: inner,
        } => match &inner.value {
            ParsedExpressionKind::Literal(ParsedLiteral::Integer(int)) => {
                Ok(ParsedExpression::new(
                    ParsedExpressionKind::Literal(ParsedLiteral::Integer(-int.clone())),
                    expr.location,
                ))
            }
            _ => Ok(expr),
        },
        _ => Ok(expr),
    }
}

fn parse_postfix_unary(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let mut expr = parse_primary_expression(tokens)?;
    let location = expr.location.clone();
    let current_module = &location.file.as_ref().unwrap().id;
    loop {
        let token = tokens.peek().clone();
        match token.value {
            Token::Static(StaticToken::Increment) => {
                tokens.shift();
                expr = ParsedExpression::new(
                    ParsedExpressionKind::Unary {
                        expr: Box::new(expr),
                        op: UnaryOp::Increment { is_prefix: false },
                    },
                    location.clone(),
                );
            }
            Token::Static(StaticToken::Decrement) => {
                tokens.shift();
                expr = ParsedExpression::new(
                    ParsedExpressionKind::Unary {
                        expr: Box::new(expr),
                        op: UnaryOp::Decrement { is_prefix: false },
                    },
                    location.clone(),
                );
            }
            Token::Static(StaticToken::Dot) => {
                tokens.shift();
                let id = parse_scoped_id(tokens, current_module).with_context(|| {
                    format!("Failed to parse identifier at {}.", token.location)
                })?;

                match tokens.peek().value {
                    Token::Static(StaticToken::OpenParen) => {}
                    Token::Static(StaticToken::DoubleColon) => {}
                    _ => {
                        if !id.is_module_local {
                            return Err(anyhow::anyhow!(
                                "Expected module local identifier at {}.",
                                token.location
                            ));
                        }
                        expr = ParsedExpression::new(
                            ParsedExpressionKind::Unary {
                                expr: Box::new(expr),
                                op: UnaryOp::Member(id.item_id.item_name),
                            },
                            token.location,
                        );
                        continue;
                    }
                }

                let generic_args =
                    parse_generic_scoped_id_extension(tokens).with_context(|| {
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

                expr = parse_function_call(tokens, var_expr, location.clone(), Some(expr))?
            }
            Token::Static(StaticToken::OpenParen) => {
                expr = parse_function_call(tokens, expr, location.clone(), None)?;
            }
            Token::Static(StaticToken::OpenBracket) => {
                tokens.shift();
                let index_expr = parse_expression(tokens)?;
                expr = ParsedExpression::new(
                    ParsedExpressionKind::Binary {
                        left: Box::new(expr),
                        right: Box::new(index_expr),
                        op: BinaryOp::Index,
                    },
                    location.clone(),
                );
                pop_expected(tokens, Token::Static(StaticToken::CloseBracket))?;
            }
            _ => break,
        }
    }
    Ok(expr)
}
