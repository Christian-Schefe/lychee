use anyhow::Context;
use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::parser_error::{LocationError, ParseResult};
use crate::compiler::parser::{parse_type, pop_or_err};
use crate::compiler::parser::syntax_tree::{SrcStructDefinition, StructDefinition};

pub(crate) fn parse_struct_definition(tokens: &mut TokenStack) -> ParseResult<SrcStructDefinition> {
    let struct_token = pop_or_err(tokens, Token::Keyword(Keyword::Struct))?;
    let location = struct_token.location.clone();
    let name_token = tokens.pop();
    let name = match &name_token.value {
        Token::Identifier(name) => name.clone(),
        _ => return Err(LocationError::msg("Expected Identifier.", &location).context("Failed to parse struct identifier.")),
    };
    pop_or_err(tokens, Token::Static(StaticToken::OpenBrace))?;
    let mut fields = Vec::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let field_type = parse_type(tokens).context("Failed to parse field type.")?;
        let field_name_token = tokens.pop();
        let field_name = match &field_name_token.value {
            Token::Identifier(name) => name.clone(),
            _ => return Err(LocationError::msg("Expected Identifier.", &location).context("Failed to parse field identifier.")),
        };
        pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
        fields.push((field_name, field_type));
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseBrace))?;
    Ok(SrcStructDefinition::new(
        StructDefinition {
            name,
            fields,
        },
        &location,
    ))
}