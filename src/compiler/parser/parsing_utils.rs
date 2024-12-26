use crate::compiler::lexer::location::Location;
use crate::compiler::lexer::token::Token;
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parser_error::ParseResult;
use crate::compiler::parser::program_parser::pop_expected;
use anyhow::Context;

pub fn parse_seperated_elements<T>(
    tokens: &mut TokenStack,
    open_token: Token,
    close_token: Token,
    separator_token: Token,
    allow_trailing: bool,
    allow_empty: bool,
    component_name: &str,
    parser_fn: impl Fn(&mut TokenStack) -> ParseResult<T>,
) -> ParseResult<(Location, Vec<T>, bool)> {
    let token = pop_expected(tokens, open_token)?;
    let mut expressions = Vec::new();
    let mut has_trailed = false;
    while tokens.peek().value != close_token {
        let expr_location = tokens.location().clone();
        let expr = parser_fn(tokens).with_context(|| {
            format!(
                "Failed to parse expression in {component_name} at {}.",
                expr_location
            )
        })?;
        expressions.push(expr);
        if tokens.peek().value == close_token {
            break;
        } else if tokens.peek().value == separator_token {
            tokens.shift();
            if tokens.peek().value == close_token {
                has_trailed = true;
                break;
            }
        } else {
            Err(anyhow::anyhow!(
                "Expected {} or {} after expression in {component_name} at {}.",
                separator_token,
                close_token,
                tokens.location().clone()
            ))?;
        }
    }
    if !allow_trailing && has_trailed {
        Err(anyhow::anyhow!(
            "Unexpected trailing {} in {component_name} at {}.",
            separator_token,
            tokens.location().clone()
        ))?;
    }
    if !allow_empty && expressions.is_empty() {
        Err(anyhow::anyhow!(
            "Expected at least one element in {component_name} at {}.",
            token.location
        ))?;
    }
    pop_expected(tokens, close_token)?;
    Ok((token.location, expressions, has_trailed))
}
