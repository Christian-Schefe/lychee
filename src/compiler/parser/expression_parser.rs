use anyhow::{Context};
use crate::compiler::lexer::{Location};
use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parser_error::{ParseResult, LocationError, consume_propagate};
use crate::compiler::parser::{parse_statement, parse_type, pop_or_err};
use crate::compiler::parser::syntax_tree::{BinaryMathOp, BinaryOp, BinaryComparisonOp, Expression, SrcExpression, UnaryOp, BinaryLogicOp, Literal};

fn parse_left_associative<F>(tokens: &mut TokenStack, op_tokens: &[(StaticToken, BinaryOp)], parse_lower: F) -> ParseResult<SrcExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<SrcExpression>,
{
    let mut expr = parse_lower(tokens)?;
    let location = expr.location.clone();
    while let Some((_, op)) = op_tokens.iter().find(|t| if let Token::Static(tkn) = &tokens.peek().value { *tkn == t.0 } else { false }) {
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

fn parse_right_associative<F>(tokens: &mut TokenStack, op_tokens: &[(StaticToken, BinaryOp)], parse_lower: F) -> ParseResult<SrcExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<SrcExpression>,
{
    let mut expr = parse_lower(tokens)?;
    let location = expr.location.clone();
    let token = &tokens.peek().value;
    if let Some((_, op)) = op_tokens.iter().find(|t| if let Token::Static(tkn) = token { *tkn == t.0 } else { false }) {
        tokens.pop();
        let right = parse_right_associative(tokens, op_tokens, parse_lower)?;
        expr = SrcExpression::new(Expression::Binary {
            left: Box::new(expr),
            op: op.clone(),
            right: Box::new(right),
        }, &location);
    }
    Ok(expr)
}

pub fn parse_expression(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_assignment_or_lower(tokens)
}

fn parse_assignment_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    parse_right_associative(tokens, &[
        (StaticToken::Assign, BinaryOp::Assign),
        (StaticToken::AddAssign, BinaryOp::MathAssign(BinaryMathOp::Add)),
        (StaticToken::SubAssign, BinaryOp::MathAssign(BinaryMathOp::Sub)),
        (StaticToken::MulAssign, BinaryOp::MathAssign(BinaryMathOp::Mul)),
        (StaticToken::DivAssign, BinaryOp::MathAssign(BinaryMathOp::Div)),
        (StaticToken::ModAssign, BinaryOp::MathAssign(BinaryMathOp::Mod)),
        (StaticToken::AndAssign, BinaryOp::MathAssign(BinaryMathOp::And)),
        (StaticToken::OrAssign, BinaryOp::MathAssign(BinaryMathOp::Or)),
        (StaticToken::XorAssign, BinaryOp::MathAssign(BinaryMathOp::Xor)),
        (StaticToken::ShiftLeftAssign, BinaryOp::MathAssign(BinaryMathOp::Shl)),
        (StaticToken::ShiftRightAssign, BinaryOp::MathAssign(BinaryMathOp::Shr)),
        (StaticToken::LogicalAndAssign, BinaryOp::LogicAssign(BinaryLogicOp::And)),
        (StaticToken::LogicalOrAssign, BinaryOp::LogicAssign(BinaryLogicOp::Or)),
    ], parse_ternary_or_lower)
}

fn parse_ternary_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let expr = parse_logical_or_or_lower(tokens)?;
    let location = expr.location.clone();
    if let Token::Static(StaticToken::QuestionMark) = tokens.peek().value {
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
    parse_left_associative(tokens, &[(StaticToken::Asterisk, BinaryOp::Math(BinaryMathOp::Mul)), (StaticToken::Slash, BinaryOp::Math(BinaryMathOp::Div)), (StaticToken::Percent, BinaryOp::Math(BinaryMathOp::Mod))], parse_prefix_unary_or_lower)
}

fn parse_prefix_unary_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let token = tokens.peek();
    let location = token.location.clone();
    if let Token::Static(static_token) = &token.value {
        let bin_op = match static_token {
            StaticToken::Plus => Some(UnaryOp::Positive),
            StaticToken::Minus => Some(UnaryOp::Negate),
            StaticToken::ExclamationMark => Some(UnaryOp::LogicalNot),
            StaticToken::Tilde => Some(UnaryOp::Not),
            _ => None
        };
        if let Some(op) = bin_op {
            tokens.pop();
            let expr = parse_prefix_unary_or_lower(tokens)?;
            return Ok(SrcExpression::new(Expression::Unary {
                op,
                expr: Box::new(expr),
            }, &location));
        } else if let StaticToken::Ampersand = static_token {
            tokens.pop();
            let expr = parse_prefix_unary_or_lower(tokens)?;
            return Ok(SrcExpression::new(Expression::Borrow(Box::new(expr)), &location));
        } else if let StaticToken::Asterisk = static_token {
            tokens.pop();
            let expr = parse_prefix_unary_or_lower(tokens)?;
            return Ok(SrcExpression::new(Expression::Dereference(Box::new(expr)), &location));
        } else if let StaticToken::Increment = static_token {
            tokens.pop();
            let expr = parse_prefix_unary_or_lower(tokens)?;
            return Ok(SrcExpression::new(Expression::Increment { expr: Box::new(expr), is_increment: true, postfix: false }, &location));
        } else if let StaticToken::Decrement = static_token {
            tokens.pop();
            let expr = parse_prefix_unary_or_lower(tokens)?;
            return Ok(SrcExpression::new(Expression::Increment { expr: Box::new(expr), is_increment: false, postfix: false }, &location));
        }
    } else if let Token::Keyword(Keyword::Sizeof) = token.value {
        tokens.pop();
        pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;
        let var_type = parse_type(tokens)?;
        pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
        return Ok(SrcExpression::new(Expression::Sizeof(var_type), &location));
    }

    parse_postfix_unary_or_lower(tokens)
}

fn parse_primary(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let location = tokens.peek().location.clone();

    let token = tokens.peek();
    match &token.value {
        Token::Static(StaticToken::OpenParen) => {
            let offset = tokens.offset;
            let struct_literal = parse_struct_literal(tokens, &location);
            if struct_literal.is_ok() {
                return struct_literal;
            }
            tokens.offset = offset;

            tokens.pop();
            if tokens.peek().value == Token::Static(StaticToken::CloseParen) {
                tokens.pop();
                return Ok(SrcExpression::new(Expression::Literal(Literal::Unit), &location));
            }

            let mut expr = parse_expression(tokens)?;
            expr.location = location;
            pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
            Ok(expr)
        }
        Token::Static(StaticToken::OpenBrace) => {
            parse_block_expr(tokens)
        }
        Token::Literal(c) => {
            let expr = SrcExpression::new(Expression::Literal(c.clone()), &location);
            tokens.pop();
            Ok(expr)
        }
        Token::String(_) => {
            unimplemented!("String literals are not yet supported")
        }
        Token::Identifier(name) => {
            let name = name.to_string();
            tokens.pop();
            match tokens.peek().value {
                Token::Static(StaticToken::OpenParen) => parse_function_call(name, tokens, &location),
                _ => Ok(SrcExpression::new(Expression::Variable(name), &location))
            }
        }
        _ => Err(LocationError::msg(&format!("Invalid Token: '{}'", token.value), &token.location)),
    }
}

fn parse_struct_literal(tokens: &mut TokenStack, location: &Location) -> ParseResult<SrcExpression> {
    pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;
    let struct_type = parse_type(tokens).context("Failed to parse struct type.")?;
    pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
    pop_or_err(tokens, Token::Static(StaticToken::OpenBrace))?;
    let mut fields = Vec::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let field_name_token = tokens.pop();
        let field_name = match &field_name_token.value {
            Token::Identifier(name) => name.clone(),
            _ => return Err(LocationError::expect("Identifier", field_name_token).context("Failed to parse field identifier.")),
        };
        pop_or_err(tokens, Token::Static(StaticToken::Colon))?;
        let field_expr = parse_expression(tokens).context("Failed to parse field expression.")?;
        fields.push((field_name, field_expr));
        if tokens.peek().value != Token::Static(StaticToken::Comma) {
            break;
        }
        tokens.pop();
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseBrace))?;
    if fields.is_empty() {
        return Err(LocationError::msg("Struct literals must have at least one field.", location).context("Failed to parse struct literal."));
    }
    Ok(SrcExpression::new(Expression::StructLiteral { struct_type, fields }, location))
}

fn parse_function_call(name: String, tokens: &mut TokenStack, location: &Location) -> ParseResult<SrcExpression> {
    pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;

    let mut args = Vec::new();
    if tokens.peek().value != Token::Static(StaticToken::CloseParen) {
        loop {
            let arg_location = tokens.peek().location.clone();
            let arg_expr = parse_expression(tokens).with_context(|| format!("Failed to parse function argument at {}.", arg_location))?;
            args.push(arg_expr);
            if tokens.peek().value != Token::Static(StaticToken::Comma) {
                break;
            }
            tokens.pop();
        }
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
    Ok(SrcExpression::new(Expression::FunctionCall { function: name, args }, &location))
}

pub fn parse_block_expr(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let location = tokens.peek().location.clone();
    pop_or_err(tokens, Token::Static(StaticToken::OpenBrace))?;
    let mut statements = Vec::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let offset = tokens.offset;
        let statement_location = tokens.peek().location.clone();
        let statement = consume_propagate(parse_statement(tokens)).with_context(|| format!("Failed to parse statement at {}.", statement_location))?;
        if let Some(statement) = statement {
            statements.push(statement);
        } else {
            tokens.offset = offset;
            let final_expr = parse_expression(tokens).with_context(|| format!("Failed to parse block at {}.", location))?;
            pop_or_err(tokens, Token::Static(StaticToken::CloseBrace)).context("Failed to parse block.")?;
            return Ok(SrcExpression::new(Expression::Block(statements, Some(Box::new(final_expr))), &location));
        }
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseBrace))?;
    Ok(SrcExpression::new(Expression::Block(statements, None), &location))
}

fn parse_postfix_unary_or_lower(tokens: &mut TokenStack) -> ParseResult<SrcExpression> {
    let mut expr = parse_primary(tokens)?;
    let location = expr.location.clone();
    loop {
        let next_token = tokens.peek();
        expr = match next_token.value {
            Token::Static(StaticToken::Increment) | Token::Static(StaticToken::Decrement) => {
                let is_increment = if let Token::Static(StaticToken::Increment) = next_token.value { true } else { false };
                tokens.pop();
                SrcExpression::new(Expression::Increment { is_increment, postfix: true, expr: Box::new(expr) }, &location)
            }
            Token::Keyword(Keyword::As) => {
                tokens.pop();
                let var_type = parse_type(tokens)?;
                SrcExpression::new(Expression::Cast { var_type, expr: Box::new(expr) }, &location)
            }
            Token::Static(StaticToken::Dot) => {
                tokens.pop();
                let next_token = tokens.pop();
                let member = match &next_token.value {
                    Token::Identifier(name) => name.clone(),
                    tkn => return Err(LocationError::msg(&format!("Expected an Identifier, found token {}", tkn), &location).context("Failed to parse member access.")),
                };
                SrcExpression::new(Expression::MemberAccess { expr: Box::new(expr), member }, &location)
            }
            _ => {
                break;
            }
        }
    }
    Ok(expr)
}