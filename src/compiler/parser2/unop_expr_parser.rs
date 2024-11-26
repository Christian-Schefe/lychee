use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser2::parsed_expression::{ParsedExpression, ParsedExpressionKind, ParsedLiteral, UnaryMathOp, UnaryOp};
use crate::compiler::parser2::parser_error::ParseResult;
use crate::compiler::parser2::primary_expr_parser::parse_primary_expression;
use crate::compiler::parser2::program_parser::{parse_expression, parse_identifier, pop_expected};
use crate::compiler::parser2::type_parser::parse_type;

fn parse_prefix_unary<F>(tokens: &mut TokenStack, op_tokens: &[(StaticToken, UnaryOp)], parse_lower: F) -> ParseResult<ParsedExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<ParsedExpression>,
{
    let token = tokens.peek().clone();
    if let Some((_, op)) = op_tokens.iter().find(|t| if let Token::Static(tkn) = &token.value { *tkn == t.0 } else { false }) {
        tokens.pop();
        let inner = parse_prefix_unary(tokens, op_tokens, parse_lower)?;
        Ok(ParsedExpression::new(ParsedExpressionKind::Unary {
            expr: Box::new(inner),
            op: op.clone(),
        }, token.location))
    } else {
        parse_lower(tokens)
    }
}

pub fn parse_unop_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let expr = parse_prefix_unary(tokens, &[
        (StaticToken::Ampersand, UnaryOp::Borrow),
        (StaticToken::Asterisk, UnaryOp::Dereference),
        (StaticToken::Minus, UnaryOp::Math(UnaryMathOp::Negate)),
        (StaticToken::Plus, UnaryOp::Math(UnaryMathOp::Positive)),
        (StaticToken::Tilde, UnaryOp::Math(UnaryMathOp::BitwiseNot)),
        (StaticToken::ExclamationMark, UnaryOp::LogicalNot),
        (StaticToken::Increment, UnaryOp::Increment { is_prefix: true }),
        (StaticToken::Decrement, UnaryOp::Decrement { is_prefix: true }),
    ], parse_postfix_unary)?;

    match &expr.value {
        ParsedExpressionKind::Unary { op: UnaryOp::Math(UnaryMathOp::Negate), expr: inner } => {
            match &inner.value {
                ParsedExpressionKind::Literal(ParsedLiteral::Integer(int)) => {
                    Ok(ParsedExpression::new(ParsedExpressionKind::Literal(ParsedLiteral::Integer(-int.clone())), expr.location))
                }
                _ => Ok(expr)
            }
        }
        _ => Ok(expr)
    }
}

fn parse_postfix_unary(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let mut expr = parse_primary_expression(tokens)?;
    loop {
        let token = tokens.peek().clone();
        match token.value {
            Token::Static(StaticToken::Increment) => {
                tokens.pop();
                expr = ParsedExpression::new(ParsedExpressionKind::Unary {
                    expr: Box::new(expr),
                    op: UnaryOp::Increment { is_prefix: false },
                }, token.location);
            }
            Token::Static(StaticToken::Decrement) => {
                tokens.pop();
                expr = ParsedExpression::new(ParsedExpressionKind::Unary {
                    expr: Box::new(expr),
                    op: UnaryOp::Decrement { is_prefix: false },
                }, token.location);
            }
            Token::Keyword(Keyword::As) => {
                tokens.pop();
                let cast_type = parse_type(tokens)?;
                expr = ParsedExpression::new(ParsedExpressionKind::Unary {
                    expr: Box::new(expr),
                    op: UnaryOp::Cast(cast_type),
                }, token.location);
            }
            Token::Static(StaticToken::Dot) => {
                tokens.pop();
                let member_name = parse_identifier(tokens)?.value;
                expr = ParsedExpression::new(ParsedExpressionKind::Unary {
                    expr: Box::new(expr),
                    op: UnaryOp::Member(member_name),
                }, token.location);
            }
            Token::Static(StaticToken::OpenBracket) => {
                tokens.pop();
                let index_expr = parse_expression(tokens)?;
                expr = ParsedExpression::new(ParsedExpressionKind::Unary {
                    expr: Box::new(expr),
                    op: UnaryOp::Index(Box::new(index_expr)),
                }, token.location);
                pop_expected(tokens, Token::Static(StaticToken::CloseBracket))?;
            }
            _ => break,
        }
    }
    Ok(expr)
}
