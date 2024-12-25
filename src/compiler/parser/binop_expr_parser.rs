use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::binary_op::{BinaryLogicOp, BinaryMathOp, BinaryOp};
use crate::compiler::parser::binop_constructor::{
    build_binop_expression, BinopElement, LEFT_ASSOCIATIVE_BINARY_OPERATORS,
};
use crate::compiler::parser::parsed_expression::{ParsedExpression, ParsedExpressionKind, UnaryOp};
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::type_parser::parse_type;
use crate::compiler::parser::unop_expr_parser::parse_unop_expression;

pub fn find_op(tokens: &TokenStack, op_tokens: &[(StaticToken, BinaryOp)]) -> Option<BinaryOp> {
    op_tokens
        .iter()
        .find(|t| {
            if let Token::Static(tkn) = &tokens.peek().value {
                *tkn == t.0
            } else {
                false
            }
        })
        .map(|t| t.1.clone())
}

pub fn parse_right_associative<F>(
    tokens: &mut TokenStack,
    op_tokens: &[(StaticToken, BinaryOp)],
    parse_lower: F,
) -> ParseResult<ParsedExpression>
where
    F: Fn(&mut TokenStack) -> ParseResult<ParsedExpression>,
{
    let mut expr = parse_lower(tokens)?;
    let location = expr.location.clone();
    if let Some(op) = find_op(tokens, op_tokens) {
        tokens.shift();
        let right = parse_right_associative(tokens, op_tokens, parse_lower)?;
        expr = ParsedExpression::new(
            ParsedExpressionKind::Binary {
                left: Box::new(expr),
                op: op.clone(),
                right: Box::new(right),
            },
            location.clone(),
        );
    }
    Ok(expr)
}

pub fn parse_binop_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_right_associative(
        tokens,
        &[
            (StaticToken::Assign, BinaryOp::Assign),
            (
                StaticToken::AddAssign,
                BinaryOp::MathAssign(BinaryMathOp::Add),
            ),
            (
                StaticToken::SubAssign,
                BinaryOp::MathAssign(BinaryMathOp::Sub),
            ),
            (
                StaticToken::MulAssign,
                BinaryOp::MathAssign(BinaryMathOp::Mul),
            ),
            (
                StaticToken::DivAssign,
                BinaryOp::MathAssign(BinaryMathOp::Div),
            ),
            (
                StaticToken::ModAssign,
                BinaryOp::MathAssign(BinaryMathOp::Mod),
            ),
            (
                StaticToken::AndAssign,
                BinaryOp::MathAssign(BinaryMathOp::And),
            ),
            (
                StaticToken::OrAssign,
                BinaryOp::MathAssign(BinaryMathOp::Or),
            ),
            (
                StaticToken::XorAssign,
                BinaryOp::MathAssign(BinaryMathOp::Xor),
            ),
            (
                StaticToken::ShiftLeftAssign,
                BinaryOp::MathAssign(BinaryMathOp::Shl),
            ),
            (
                StaticToken::ShiftRightAssign,
                BinaryOp::MathAssign(BinaryMathOp::Shr),
            ),
            (
                StaticToken::LogicalAndAssign,
                BinaryOp::LogicAssign(BinaryLogicOp::And),
            ),
            (
                StaticToken::LogicalOrAssign,
                BinaryOp::LogicAssign(BinaryLogicOp::Or),
            ),
        ],
        parse_left_associative,
    )
}

pub fn parse_left_associative(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let expr = parse_cast_or_lower(tokens)?;
    let mut elements = vec![BinopElement::Expr(expr)];

    while let Some(op) = find_op(tokens, &LEFT_ASSOCIATIVE_BINARY_OPERATORS) {
        tokens.shift();
        let right = parse_cast_or_lower(tokens)?;
        elements.push(BinopElement::Op(op));
        elements.push(BinopElement::Expr(right));
    }

    if elements.len() == 1 {
        match elements.pop().unwrap() {
            BinopElement::Expr(expr) => Ok(expr),
            _ => unreachable!(),
        }
    } else {
        build_binop_expression(elements)
    }
}

pub fn parse_cast_or_lower(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    let mut expr = parse_unop_expression(tokens)?;
    let location = expr.location.clone();
    loop {
        let token = tokens.peek().clone();
        match token.value {
            Token::Keyword(Keyword::As) => {
                tokens.shift();
                let cast_type = parse_type(tokens)?;
                expr = ParsedExpression::new(
                    ParsedExpressionKind::Unary {
                        expr: Box::new(expr),
                        op: UnaryOp::Cast(cast_type),
                    },
                    location.clone(),
                );
            }
            _ => break,
        }
    }
    Ok(expr)
}
