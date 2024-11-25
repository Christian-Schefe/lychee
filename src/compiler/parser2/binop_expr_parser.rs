use crate::compiler::lexer::token::{StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser2::parsed_expression::{ParsedExpression, ParsedExpressionKind};
use crate::compiler::parser2::parser_error::ParseResult;
use crate::compiler::parser2::parsed_expression::{BinaryComparisonOp, BinaryLogicOp, BinaryMathOp, BinaryOp};
use crate::compiler::parser2::unop_expr_parser::parse_unop_expression;

fn parse_left_associative<F>(tokens: &mut TokenStack, op_tokens: &[(StaticToken, BinaryOp)], parse_lower: F) -> ParseResult<ParsedExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<ParsedExpression>,
{
    let mut expr = parse_lower(tokens)?;
    let location = expr.location.clone();
    while let Some((_, op)) = op_tokens.iter().find(|t| if let Token::Static(tkn) = &tokens.peek().value { *tkn == t.0 } else { false }) {
        tokens.pop();
        let right = parse_lower(tokens)?;
        expr = ParsedExpression::new(ParsedExpressionKind::Binary {
            left: Box::new(expr),
            op: op.clone(),
            right: Box::new(right),
        }, location.clone());
    }
    Ok(expr)
}

fn parse_right_associative<F>(tokens: &mut TokenStack, op_tokens: &[(StaticToken, BinaryOp)], parse_lower: F) -> ParseResult<ParsedExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<ParsedExpression>,
{
    let mut expr = parse_lower(tokens)?;
    let location = expr.location.clone();
    let token = &tokens.peek().value;
    if let Some((_, op)) = op_tokens.iter().find(|t| if let Token::Static(tkn) = token { *tkn == t.0 } else { false }) {
        tokens.pop();
        let right = parse_right_associative(tokens, op_tokens, parse_lower)?;
        expr = ParsedExpression::new(ParsedExpressionKind::Binary {
            left: Box::new(expr),
            op: op.clone(),
            right: Box::new(right),
        }, location.clone());
    }
    Ok(expr)
}

pub fn parse_binop_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
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
    ], parse_logical_or_or_lower)
}

fn parse_logical_or_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::LogicalOr, BinaryOp::Logical(BinaryLogicOp::Or))], parse_logical_and_or_lower)
}

fn parse_logical_and_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::LogicalAnd, BinaryOp::Logical(BinaryLogicOp::And))], parse_bitwise_or_or_lower)
}

fn parse_bitwise_or_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::Pipe, BinaryOp::Math(BinaryMathOp::Or))], parse_bitwise_xor_or_lower)
}

fn parse_bitwise_xor_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::Caret, BinaryOp::Math(BinaryMathOp::Xor))], parse_bitwise_and_or_lower)
}

fn parse_bitwise_and_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::Ampersand, BinaryOp::Math(BinaryMathOp::And))], parse_equality_or_lower)
}

fn parse_equality_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::Equals, BinaryOp::Comparison(BinaryComparisonOp::Equals)), (StaticToken::NotEquals, BinaryOp::Comparison(BinaryComparisonOp::NotEquals))], parse_relational_or_lower)
}

fn parse_relational_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::LessThan, BinaryOp::Comparison(BinaryComparisonOp::Less)), (StaticToken::GreaterThan, BinaryOp::Comparison(BinaryComparisonOp::Greater)), (StaticToken::LessThanOrEqual, BinaryOp::Comparison(BinaryComparisonOp::LessEquals)), (StaticToken::GreaterThanOrEqual, BinaryOp::Comparison(BinaryComparisonOp::GreaterEquals))], parse_shift_or_lower)
}

fn parse_shift_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::ShiftLeft, BinaryOp::Math(BinaryMathOp::Shl)), (StaticToken::ShiftRight, BinaryOp::Math(BinaryMathOp::Shr))], parse_add_or_lower)
}

fn parse_add_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::Plus, BinaryOp::Math(BinaryMathOp::Add)), (StaticToken::Minus, BinaryOp::Math(BinaryMathOp::Sub))], parse_mul_or_lower)
}

fn parse_mul_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_left_associative(tokens, &[(StaticToken::Asterisk, BinaryOp::Math(BinaryMathOp::Mul)), (StaticToken::Slash, BinaryOp::Math(BinaryMathOp::Div)), (StaticToken::Percent, BinaryOp::Math(BinaryMathOp::Mod))], parse_unop_expression)
}