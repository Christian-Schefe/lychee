use crate::compiler::lexer::Location;
use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parser_error::{ParseResult, LocationError};
use crate::compiler::parser::{parse_statement, parse_type, pop_or_err};
use crate::compiler::parser::syntax_tree::{BinaryMathOp, BinaryOp, BinaryComparisonOp, Expression, SrcExpression, UnaryOp, ASSIGN_OP_MAP, BinaryLogicOp};

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
        pop_or_err(tokens, Token::Static(StaticToken::Colon))?;
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
    parse_left_associative(tokens, &[(StaticToken::LogicalOr, BinaryOp::Logical(BinaryLogicOp::Or))], parse_logical_and_or_lower)
}

fn parse_logical_and_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::LogicalAnd, BinaryOp::Logical(BinaryLogicOp::And))], parse_bitwise_or_or_lower)
}

fn parse_bitwise_or_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Pipe, BinaryOp::Math(BinaryMathOp::Or))], parse_bitwise_xor_or_lower)
}

fn parse_bitwise_xor_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Caret, BinaryOp::Math(BinaryMathOp::Xor))], parse_bitwise_and_or_lower)
}

fn parse_bitwise_and_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Ampersand, BinaryOp::Math(BinaryMathOp::And))], parse_equality_or_lower)
}

fn parse_equality_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Equals, BinaryOp::Comparison(BinaryComparisonOp::Equals)), (StaticToken::NotEquals, BinaryOp::Comparison(BinaryComparisonOp::NotEquals))], parse_relational_or_lower)
}

fn parse_relational_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::LessThan, BinaryOp::Comparison(BinaryComparisonOp::Less)), (StaticToken::GreaterThan, BinaryOp::Comparison(BinaryComparisonOp::Greater)), (StaticToken::LessThanOrEqual, BinaryOp::Comparison(BinaryComparisonOp::LessEquals)), (StaticToken::GreaterThanOrEqual, BinaryOp::Comparison(BinaryComparisonOp::GreaterEquals))], parse_shift_or_lower)
}

fn parse_shift_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::ShiftLeft, BinaryOp::Math(BinaryMathOp::Shl)), (StaticToken::ShiftRight, BinaryOp::Math(BinaryMathOp::Shr))], parse_add_or_lower)
}

fn parse_add_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Plus, BinaryOp::Math(BinaryMathOp::Add)), (StaticToken::Minus, BinaryOp::Math(BinaryMathOp::Sub))], parse_mul_or_lower)
}

fn parse_mul_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_left_associative(tokens, &[(StaticToken::Asterisk, BinaryOp::Math(BinaryMathOp::Mul)), (StaticToken::Slash, BinaryOp::Math(BinaryMathOp::Div)), (StaticToken::Percent, BinaryOp::Math(BinaryMathOp::Mod))], parse_unary_or_lower)
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
            StaticToken::Increment => Some(UnaryOp::Increment),
            StaticToken::Decrement => Some(UnaryOp::Decrement),
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

    let token = tokens.peek();
    let location = token.location.clone();
    match &token.token {
        Token::Static(StaticToken::OpenParen) => {
            let offset = tokens.offset;
            let cast_expr = parse_cast(&location, tokens);
            if cast_expr.is_ok() {
                return cast_expr;
            }
            tokens.offset = offset;
            tokens.pop();
            let mut expr = parse_expression(tokens)?;
            expr.location = location;
            pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
            Ok(expr)
        }
        Token::Static(StaticToken::OpenBrace) => parse_block_expr(tokens),
        Token::Literal(c) => {
            let expr = SrcExpression::new(Expression::Literal(c.clone()), &location);
            tokens.pop();
            Ok(expr)
        }
        Token::String(_) => {
            unimplemented!("String literals are not yet supported")
        }
        Token::Identifier(name) => {
            let expr = SrcExpression::new(Expression::Variable(name.to_string()), &location);
            tokens.pop();
            Ok(expr)
        }
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

pub fn parse_block_expr(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let location = tokens.peek().location.clone();
    pop_or_err(tokens, Token::Static(StaticToken::OpenBrace))?;
    let mut statements = Vec::new();
    while tokens.peek().token != Token::Static(StaticToken::CloseBrace) {
        let offset = tokens.offset;
        let statement = parse_statement(tokens);
        if statement.is_err() {
            tokens.offset = offset;
            let final_expr = parse_expression(tokens).map_err(|_| statement.unwrap_err())?;
            pop_or_err(tokens, Token::Static(StaticToken::CloseBrace))?;
            return Ok(SrcExpression::new(Expression::Block(statements, Some(Box::new(final_expr))), &location));
        }
        statements.push(statement?);
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseBrace))?;
    Ok(SrcExpression::new(Expression::Block(statements, None), &location))
}

fn parse_cast(location: &Location, tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;
    let var_type = parse_type(tokens)?;
    pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
    let expr = parse_unary_or_lower(tokens)?;
    Ok(SrcExpression::new(Expression::Cast { var_type, expr: Box::new(expr) }, location))
}