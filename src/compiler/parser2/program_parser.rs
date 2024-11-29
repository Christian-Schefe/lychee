use std::collections::{HashMap, HashSet};
use anyhow::Context;
use crate::compiler::lexer::lexer_error::LocationError;
use crate::compiler::lexer::location::Src;
use crate::compiler::lexer::SrcToken;
use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser2::binop_expr_parser::{parse_binop_expression};
use crate::compiler::parser2::parsed_expression::{ParsedExpression, ParsedFunction, ParsedProgram, ParsedStructDefinition};
use crate::compiler::parser2::parser_error::ParseResult;
use crate::compiler::parser2::type_parser::parse_type;

pub fn pop_expected(tokens: &mut TokenStack, expected: Token) -> ParseResult<SrcToken> {
    let token = tokens.pop().clone();
    if token.value != expected {
        Err(LocationError::new(format!("Expected token '{}', found token '{}'", expected, token.value), token.location))?
    } else {
        Ok(token)
    }
}

pub fn pop_matching<F, T, F2>(tokens: &mut TokenStack, expected: F, error: F2) -> ParseResult<Src<T>>
where
    F: Fn(&Token) -> Option<T>,
    F2: Fn(Token) -> String,
{
    let token = tokens.pop().clone();
    if let Some(result) = expected(&token.value) {
        Ok(Src { value: result, location: token.location })
    } else {
        Err(LocationError::new(error(token.value), token.location))?
    }
}

pub fn parse_identifier(tokens: &mut TokenStack) -> ParseResult<Src<String>> {
    pop_matching(tokens, |t| {
        if let Token::Identifier(name) = t { Some(name.clone()) } else { None }
    }, |t| format!("Expected Identifier, found '{}'", t))
}

pub fn parse_program(tokens: &mut TokenStack) -> ParseResult<ParsedProgram> {
    let mut functions = Vec::new();
    let mut struct_definitions = Vec::new();

    while tokens.peek().value != Token::EOF {
        let token = tokens.peek().clone();
        match token.value {
            Token::Keyword(Keyword::Struct) => {
                let struct_def = parse_struct_definition(tokens).with_context(|| format!("Failed to parse struct definition at {}.", token.location))?;
                struct_definitions.push(struct_def);
            }
            _ => {
                let func = parse_function(tokens).with_context(|| format!("Failed to parse function at {}.", token.location))?;
                functions.push(func);
            }
        }
    }

    Ok(ParsedProgram { functions, struct_definitions })
}

pub fn parse_struct_definition(tokens: &mut TokenStack) -> ParseResult<Src<ParsedStructDefinition>> {
    let location = pop_expected(tokens, Token::Keyword(Keyword::Struct))?.location;
    let struct_name = parse_identifier(tokens)?.value;
    pop_expected(tokens, Token::Static(StaticToken::OpenBrace))?;

    let mut fields = HashMap::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseBrace) {
        let location = tokens.peek().location.clone();
        let field_type = parse_type(tokens).with_context(|| format!("Failed to parse type at {}.", location))?;
        let field_name = parse_identifier(tokens)?.value;
        pop_expected(tokens, Token::Static(StaticToken::Semicolon))?;
        if fields.insert(field_name.clone(), field_type).is_some() {
            Err(LocationError::new(format!("Duplicate field name '{}'", field_name), location))?;
        }
    }
    pop_expected(tokens, Token::Static(StaticToken::CloseBrace))?;
    if fields.is_empty() {
        Err(anyhow::anyhow!("Structs must have at least one field at {location}."))?;
    }

    Ok(Src::new(ParsedStructDefinition {
        struct_name,
        fields,
    }, location))
}

pub fn parse_function(tokens: &mut TokenStack) -> ParseResult<Src<ParsedFunction>> {
    let location = tokens.location().clone();
    let return_type = parse_type(tokens).with_context(|| format!("Failed to parse return type at {}.", location))?;
    let function_name = parse_identifier(tokens)?.value;
    pop_expected(tokens, Token::Static(StaticToken::OpenParen))?;

    let mut args = Vec::new();
    let mut used_arg_names = HashSet::new();
    while tokens.peek().value != Token::Static(StaticToken::CloseParen) {
        let location = tokens.location().clone();
        let arg_type = parse_type(tokens).with_context(|| format!("Failed to parse argument type at {}.", location))?;
        let arg_name = parse_identifier(tokens)?.value;
        if !used_arg_names.insert(arg_name.clone()) {
            Err(LocationError::new(format!("Duplicate argument name '{}'", arg_name), location))?;
        }
        args.push((arg_type, arg_name));
        match tokens.peek().value {
            Token::Static(StaticToken::Comma) => { tokens.pop(); }
            Token::Static(StaticToken::CloseParen) => {}
            _ => Err(LocationError::new(format!("Expected ',' or ')', found '{}'", tokens.peek().value), tokens.location().clone()))?,
        }
    }
    pop_expected(tokens, Token::Static(StaticToken::CloseParen))?;

    let body_location = tokens.location().clone();
    let body = parse_expression(tokens).with_context(|| format!("Failed to parse function body at {}.", body_location))?;

    Ok(Src::new(ParsedFunction {
        function_name,
        return_type,
        args,
        body,
    }, location))
}

pub fn parse_expression(tokens: &mut TokenStack) -> ParseResult<ParsedExpression> {
    parse_binop_expression(tokens)
}