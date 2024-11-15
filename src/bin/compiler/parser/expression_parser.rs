use crate::lexer::Token;
use crate::lexer::token_stack::TokenStack;
use crate::parser::parser_error::{ParseResult, ParserError};
use crate::parser::pop_or_err;
use crate::parser::syntax_tree::{BinaryOp, Expression, Literal, UnaryOp};

pub fn parse_expression(tokens: &mut TokenStack) -> ParseResult<Expression> {
    let offset = tokens.offset;
    if let Ok(result) = parse_assignment(tokens) {
        return Ok(result);
    }
    tokens.offset = offset;
    parse_ternary_or_lower(tokens)
}

fn parse_assignment(tokens: &mut TokenStack) -> ParseResult<Expression> {
    let id_token = tokens.pop();
    let id = match &id_token.token {
        Token::Identifier(name) => name,
        _ => return Err(ParserError::expect("Identifier", &id_token))
    }.to_string();
    let tokens_to_op = [
        (Token::Assign, BinaryOp::AddAssign),
        (Token::PlusAssign, BinaryOp::AddAssign),
        (Token::MinusAssign, BinaryOp::SubAssign),
        (Token::StarAssign, BinaryOp::MulAssign),
        (Token::SlashAssign, BinaryOp::DivAssign),
        (Token::PercentAssign, BinaryOp::ModAssign),
        (Token::AmpersandAssign, BinaryOp::AndAssign),
        (Token::PipeAssign, BinaryOp::OrAssign),
        (Token::CaretAssign, BinaryOp::XorAssign),
        (Token::LeftShiftAssign, BinaryOp::ShlAssign),
        (Token::RightShiftAssign, BinaryOp::ShrAssign)
    ];
    let token = tokens.peek();
    if let Some((_, op)) = tokens_to_op.iter().find(|t| t.0 == token.token) {
        tokens.pop();
        let right = parse_expression(tokens)?;
        Ok(Expression::Binary {
            left: Box::new(Expression::Variable(id)),
            op: op.clone(),
            right: Box::new(right),
        })
    } else {
        Err(ParserError::expect("Assignment operator", tokens.peek()))
    }
}

fn parse_ternary_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    let expr = parse_logical_or_or_lower(tokens)?;
    if tokens.peek().token == Token::QuestionMark {
        tokens.pop();
        let true_expr = parse_expression(tokens)?;
        let false_expr = parse_ternary_or_lower(tokens)?;
        return Ok(Expression::Ternary {
            condition: Box::new(expr),
            true_expr: Box::new(true_expr),
            false_expr: Box::new(false_expr),
        });
    }
    Ok(expr)
}

fn parse_left_associative<F>(tokens: &mut TokenStack, op_tokens: &[(Token, BinaryOp)], parse_lower: F) -> ParseResult<Expression>
where
    F: Fn(&mut TokenStack) -> ParseResult<Expression>,
{
    let mut expr = parse_lower(tokens)?;
    while let Some((_, op)) = op_tokens.iter().find(|t| t.0 == tokens.peek().token) {
        tokens.pop();
        let right = parse_lower(tokens)?;
        expr = Expression::Binary {
            left: Box::new(expr),
            op: op.clone(),
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn parse_logical_or_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::LogicalOr, BinaryOp::LogicalOr)], parse_logical_and_or_lower)
}

fn parse_logical_and_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::LogicalAnd, BinaryOp::LogicalAnd)], parse_bitwise_or_or_lower)
}

fn parse_bitwise_or_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::Pipe, BinaryOp::Or)], parse_bitwise_xor_or_lower)
}

fn parse_bitwise_xor_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::Caret, BinaryOp::Xor)], parse_bitwise_and_or_lower)
}

fn parse_bitwise_and_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::Ampersand, BinaryOp::And)], parse_equality_or_lower)
}

fn parse_equality_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::Equal, BinaryOp::Equals), (Token::NotEqual, BinaryOp::NotEquals)], parse_relational_or_lower)
}

fn parse_relational_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::LessThan, BinaryOp::Less), (Token::GreaterThan, BinaryOp::Greater), (Token::LessThanOrEqual, BinaryOp::LessEquals), (Token::GreaterThanOrEqual, BinaryOp::GreaterEquals)], parse_shift_or_lower)
}

fn parse_shift_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::ShiftLeft, BinaryOp::Shl), (Token::ShiftRight, BinaryOp::Shr)], parse_add_or_lower)
}

fn parse_add_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::Plus, BinaryOp::Add), (Token::Minus, BinaryOp::Sub)], parse_mul_or_lower)
}

fn parse_mul_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    parse_left_associative(tokens, &[(Token::Star, BinaryOp::Mul), (Token::Slash, BinaryOp::Div), (Token::Percent, BinaryOp::Mod)], parse_unary_or_lower)
}

fn parse_unary_or_lower(tokens: &mut TokenStack) -> ParseResult<Expression> {
    let token = tokens.peek();
    match token.token {
        Token::Plus | Token::Minus | Token::ExclamationMark | Token::Tilde => {
            let bin_op = match token.token {
                Token::Plus => UnaryOp::Positive,
                Token::Minus => UnaryOp::Negate,
                Token::ExclamationMark => UnaryOp::LogicalNot,
                Token::Tilde => UnaryOp::Not,
                _ => unreachable!(),
            };
            tokens.pop();
            let expr = parse_unary_or_lower(tokens)?;
            Ok(Expression::Unary {
                op: bin_op,
                expr: Box::new(expr),
            })
        }
        _ => parse_primary(tokens)
    }
}

fn parse_primary(tokens: &mut TokenStack) -> ParseResult<Expression> {
    let offset = tokens.offset;
    if let Ok(result) = parse_function_call(tokens) {
        return Ok(result);
    }
    tokens.offset = offset;

    let token = tokens.pop();
    match &token.token {
        Token::OpenParen => {
            let expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::CloseParen)?;
            Ok(expr)
        }
        Token::Integer(int) => Ok(Expression::Literal(Literal::Long(*int))),
        Token::String(string) => Ok(Expression::Literal(Literal::String(string.to_string()))),
        Token::Identifier(name) => Ok(Expression::Variable(name.to_string())),
        _ => Err(ParserError::expect("Primary expression", token)),
    }
}

fn parse_function_call(tokens: &mut TokenStack) -> ParseResult<Expression> {
    let id_token = tokens.pop();
    let id = match &id_token.token {
        Token::Identifier(name) => name,
        _ => return Err(ParserError::expect("Identifier", &id_token))
    }.to_string();

    pop_or_err(tokens, Token::OpenParen)?;

    let mut args = Vec::new();
    if tokens.peek().token != Token::CloseParen {
        loop {
            args.push(parse_expression(tokens)?);
            if tokens.peek().token != Token::Comma {
                break;
            }
            tokens.pop();
        }
    }
    pop_or_err(tokens, Token::CloseParen)?;
    Ok(Expression::FunctionCall { function: id, args })
}