use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parsed_expression::{
    BinaryOp, ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryMathOp, UnaryOp,
};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::primary_expr_parser::{parse_function_call, parse_primary_expression};
use crate::compiler::parser::program_parser::{parse_expression, parse_identifier, pop_expected};

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
        tokens.pop();
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
                tokens.pop();
                expr = ParsedExpression::new(
                    ParsedExpressionKind::Unary {
                        expr: Box::new(expr),
                        op: UnaryOp::Increment { is_prefix: false },
                    },
                    location.clone(),
                );
            }
            Token::Static(StaticToken::Decrement) => {
                tokens.pop();
                expr = ParsedExpression::new(
                    ParsedExpressionKind::Unary {
                        expr: Box::new(expr),
                        op: UnaryOp::Decrement { is_prefix: false },
                    },
                    location.clone(),
                );
            }
            Token::Static(StaticToken::Dot) => {
                tokens.pop();
                match &tokens.peek().value {
                    Token::Identifier(name) => {
                        let member_name = name.clone();
                        tokens.pop();
                        match tokens.peek().value {
                            Token::Static(StaticToken::OpenParen) => {}
                            Token::Static(StaticToken::DoubleColon) => {}
                            _ => {
                                expr = ParsedExpression::new(
                                    ParsedExpressionKind::Unary {
                                        expr: Box::new(expr),
                                        op: UnaryOp::Member(member_name),
                                    },
                                    location.clone(),
                                );
                                continue;
                            }
                        }

                        expr = parse_function_call(
                            tokens,
                            member_name,
                            false,
                            current_module,
                            location.clone(),
                            Some(expr),
                        )?;
                    }
                    Token::Static(StaticToken::DoubleColon) => {
                        tokens.pop();
                        let member_name = parse_identifier(tokens)?.value;
                        expr = parse_function_call(
                            tokens,
                            member_name,
                            true,
                            current_module,
                            location.clone(),
                            Some(expr),
                        )?;
                    }
                    _ => {
                        return Err(anyhow::anyhow!(
                            "Expected identifier or '::', found '{}'",
                            tokens.peek().value
                        ))?;
                    }
                }
            }
            Token::Static(StaticToken::OpenBracket) => {
                tokens.pop();
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
