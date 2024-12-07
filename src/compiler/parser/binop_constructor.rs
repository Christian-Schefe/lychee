use crate::compiler::lexer::token::StaticToken;
use crate::compiler::parser::parsed_expression::{
    BinaryComparisonOp, BinaryLogicOp, BinaryMathOp, BinaryOp, ParsedExpression,
    ParsedExpressionKind,
};
use crate::compiler::parser::parser_error::ParseResult;
use lazy_static::lazy_static;

lazy_static! {
    pub static ref LEFT_ASSOCIATIVE_BINARY_OPERATORS: Vec<(StaticToken, BinaryOp)> = {
        vec![
            (StaticToken::LogicalOr, BinaryOp::Logical(BinaryLogicOp::Or)),
            (
                StaticToken::LogicalAnd,
                BinaryOp::Logical(BinaryLogicOp::And),
            ),
            (StaticToken::Pipe, BinaryOp::Math(BinaryMathOp::Or)),
            (StaticToken::Caret, BinaryOp::Math(BinaryMathOp::Xor)),
            (StaticToken::Ampersand, BinaryOp::Math(BinaryMathOp::And)),
            (
                StaticToken::Equals,
                BinaryOp::Comparison(BinaryComparisonOp::Equals),
            ),
            (
                StaticToken::NotEquals,
                BinaryOp::Comparison(BinaryComparisonOp::NotEquals),
            ),
            (
                StaticToken::LessThan,
                BinaryOp::Comparison(BinaryComparisonOp::Less),
            ),
            (
                StaticToken::GreaterThan,
                BinaryOp::Comparison(BinaryComparisonOp::Greater),
            ),
            (
                StaticToken::LessThanOrEqual,
                BinaryOp::Comparison(BinaryComparisonOp::LessEquals),
            ),
            (
                StaticToken::GreaterThanOrEqual,
                BinaryOp::Comparison(BinaryComparisonOp::GreaterEquals),
            ),
            (StaticToken::ShiftLeft, BinaryOp::Math(BinaryMathOp::Shl)),
            (StaticToken::ShiftRight, BinaryOp::Math(BinaryMathOp::Shr)),
            (StaticToken::Plus, BinaryOp::Math(BinaryMathOp::Add)),
            (StaticToken::Minus, BinaryOp::Math(BinaryMathOp::Sub)),
            (StaticToken::Asterisk, BinaryOp::Math(BinaryMathOp::Mul)),
            (StaticToken::Slash, BinaryOp::Math(BinaryMathOp::Div)),
            (StaticToken::Percent, BinaryOp::Math(BinaryMathOp::Mod)),
        ]
    };
}

#[derive(Debug, Clone)]
pub enum BinopElement {
    Expr(ParsedExpression),
    Op(BinaryOp),
}

pub fn build_binop_expression(elements: Vec<BinopElement>) -> ParseResult<ParsedExpression> {
    let postfix = infix_to_postfix(elements);
    postfix_to_tree(postfix)
}

fn infix_to_postfix(elements: Vec<BinopElement>) -> Vec<BinopElement> {
    let mut stack: Vec<BinaryOp> = Vec::new();
    let mut output: Vec<BinopElement> = Vec::new();
    for element in elements {
        match &element {
            BinopElement::Expr(_) => {
                output.push(element);
            }
            BinopElement::Op(op) => {
                let precedence = op.precedence();
                while stack.len() > 0 && stack.last().unwrap().precedence() <= precedence {
                    let op = stack.pop().unwrap();
                    output.push(BinopElement::Op(op));
                }
                stack.push(op.clone());
            }
        }
    }
    while stack.len() > 0 {
        let op = stack.pop().unwrap();
        output.push(BinopElement::Op(op));
    }

    output
}

fn postfix_to_tree(elements: Vec<BinopElement>) -> ParseResult<ParsedExpression> {
    let mut stack: Vec<ParsedExpression> = Vec::new();
    for element in elements {
        match element {
            BinopElement::Expr(expr) => {
                stack.push(expr);
            }
            BinopElement::Op(op) => {
                let right = stack.pop().unwrap();
                let left = stack.pop().unwrap();
                let new_expr = ParsedExpression {
                    location: left.location.clone(),
                    value: ParsedExpressionKind::Binary {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                };
                stack.push(new_expr);
            }
        }
    }
    if stack.len() != 1 {
        unreachable!("Invalid postfix expression");
    } else {
        Ok(stack.pop().unwrap())
    }
}
