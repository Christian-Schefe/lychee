use crate::lexer::Location;
use crate::lexer::token::{StaticToken, Token};
use crate::lexer::token_stack::TokenStack;
use crate::parser::parser_error::{ParseResult, LocationError};
use crate::parser::{parse_statement, pop_or_err};
use crate::parser::syntax_tree::{BinaryOp, Expression, Literal, SrcExpression, UnaryOp, ASSIGN_OP_MAP};

pub fn parse_expression(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let offset = tokens.offset;
    if let Ok(result) = parse_assignment(tokens) {
        return Ok(result);
    }
    tokens.offset = offset;
    parse_ternary_or_lower(tokens)
}

fn parse_assignment(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let id_token = tokens.pop();
    let id = match &id_token.token {
        Token::Identifier(name) => name,
        _ => return Err(LocationError::expect("Identifier", id_token))
    }.to_string();

    let token = tokens.peek();
    let location = token.location.clone();
    if let Token::Static(static_token) = &token.token {
        if let Some(op) = ASSIGN_OP_MAP.get(static_token) {
            tokens.pop();
            let right = parse_expression(tokens)?;
            Ok(SrcExpression::new(Expression::Binary {
                left: Box::new(SrcExpression::new(Expression::Variable(id), &location)),
                op: op.clone(),
                right: Box::new(right),
            }, &location))
        } else {
            Err(LocationError::expect("Assignment operator", tokens.peek()))
        }
    } else {
        Err(LocationError::expect("Assignment operator", tokens.peek()))
    }
}

fn parse_ternary_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let expr = parse_logical_or_or_lower(tokens)?;
    let location = expr.location.clone();
    if let Token::Static(StaticToken::QuestionMark) = tokens.peek().token {
        tokens.pop();
        let true_expr = parse_expression(tokens)?;
        let false_expr = parse_ternary_or_lower(tokens)?;
        return Ok(SrcExpression::new(Expression::Ternary {
            condition: Box::new(expr),
            true_expr: Box::new(true_expr),
            false_expr: Box::new(false_expr),
        }, &location));
    }
    Ok(expr)
}

fn parse_left_associative<F>(tokens: &mut TokenStack, op_tokens: &[(StaticToken, BinaryOp)], parse_lower: F) -> ParseResult<SrcExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<SrcExpression>,
{
    let mut expr = parse_lower(tokens)?;
    let location = expr.location.clone();
    while let Some((_, op)) = op_tokens.iter().find(|t| if let Token::Static(tkn) = &tokens.peek().token { *tkn == t.0 } else { false }) {
        tokens.pop();
        let right = parse_lower(tokens)?;
        expr = SrcExpression::new(Expression::Binary {
            left: Box::new(expr),
            op: op.clone(),
            right: Box::new(right),
        }, &location);
    }
    Ok(expr)
}

fn parse_logical_or_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::LogicalOr, BinaryOp::LogicalOr)], parse_logical_and_or_lower)
}

fn parse_logical_and_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::LogicalAnd, BinaryOp::LogicalAnd)], parse_bitwise_or_or_lower)
}

fn parse_bitwise_or_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Pipe, BinaryOp::Or)], parse_bitwise_xor_or_lower)
}

fn parse_bitwise_xor_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Caret, BinaryOp::Xor)], parse_bitwise_and_or_lower)
}

fn parse_bitwise_and_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Ampersand, BinaryOp::And)], parse_equality_or_lower)
}

fn parse_equality_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Equals, BinaryOp::Equals), (StaticToken::NotEquals, BinaryOp::NotEquals)], parse_relational_or_lower)
}

fn parse_relational_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::LessThan, BinaryOp::Less), (StaticToken::GreaterThan, BinaryOp::Greater), (StaticToken::LessThanOrEqual, BinaryOp::LessEquals), (StaticToken::GreaterThanOrEqual, BinaryOp::GreaterEquals)], parse_shift_or_lower)
}

fn parse_shift_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::ShiftLeft, BinaryOp::Shl), (StaticToken::ShiftRight, BinaryOp::Shr)], parse_add_or_lower)
}

fn parse_add_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Plus, BinaryOp::Add), (StaticToken::Minus, BinaryOp::Sub)], parse_mul_or_lower)
}

fn parse_mul_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Asterisk, BinaryOp::Mul), (StaticToken::Slash, BinaryOp::Div), (StaticToken::Percent, BinaryOp::Mod)], parse_unary_or_lower)
}

fn parse_unary_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let token = tokens.peek();
    let location = token.location.clone();
    if let Token::Static(static_token) = &token.token {
        let bin_op = match static_token {
            StaticToken::Plus => Some(UnaryOp::Positive),
            StaticToken::Minus => Some(UnaryOp::Negate),
            StaticToken::ExclamationMark => Some(UnaryOp::LogicalNot),
            StaticToken::Tilde => Some(UnaryOp::Not),
            _ => None
        };
        if let Some(op) = bin_op {
            tokens.pop();
            let expr = parse_unary_or_lower(tokens)?;
            return Ok(SrcExpression::new(Expression::Unary {
                op,
                expr: Box::new(expr),
            }, &location));
        }
    }

    parse_primary(tokens)
}

fn parse_primary(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let offset = tokens.offset;
    if let Ok(result) = parse_function_call(tokens) {
        return Ok(result);
    }
    tokens.offset = offset;

    let token = tokens.pop();
    let location = token.location.clone();
    match &token.token {
        Token::Static(StaticToken::OpenParen) => {
            let expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
            Ok(expr)
        }
        Token::Static(StaticToken::OpenBrace) => parse_block(&location, tokens),
        Token::Integer(int) => Ok(SrcExpression::new(Expression::Literal(Literal::Int(*int)), &location)),
        Token::Long(long) => Ok(SrcExpression::new(Expression::Literal(Literal::Long(*long)), &location)),
        Token::String(string) => Ok(SrcExpression::new(Expression::Literal(Literal::String(string.to_string())), &location)),
        Token::Identifier(name) => Ok(SrcExpression::new(Expression::Variable(name.to_string()), &location)),
        _ => Err(LocationError::expect("Primary expression", token)),
    }
}

fn parse_function_call(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let id_token = tokens.pop();
    let location = id_token.location.clone();
    let id = match &id_token.token {
        Token::Identifier(name) => name,
        _ => return Err(LocationError::expect("Identifier", id_token))
    }.to_string();

    pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;

    let mut args = Vec::new();
    if tokens.peek().token != Token::Static(StaticToken::CloseParen) {
        loop {
            args.push(parse_expression(tokens)?);
            if tokens.peek().token != Token::Static(StaticToken::Comma) {
                break;
            }
            tokens.pop();
        }
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
    Ok(SrcExpression::new(Expression::FunctionCall { function: id, args }, &location))
}

fn parse_block(location: &Location, tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let mut statements = Vec::new();
    while tokens.peek().token != Token::Static(StaticToken::CloseBrace) {
        let statement = parse_statement(tokens);
        if statement.is_err() {
            let final_expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::Static(StaticToken::CloseBrace))?;
            return Ok(SrcExpression::new(Expression::Block(statements, Some(Box::new(final_expr))), location));
        }
        statements.push(statement?);
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseBrace))?;
    Ok(SrcExpression::new(Expression::Block(statements, None), location))
}