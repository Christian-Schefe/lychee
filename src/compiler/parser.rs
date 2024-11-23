use anyhow::Context;
use crate::compiler::lexer::{Location, SrcToken};
use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::expression_parser::{parse_block_expr, parse_expression};
use crate::compiler::parser::parser_error::{LocationError, ParseResult};
use crate::compiler::parser::struct_parser::parse_struct_definition;
use crate::compiler::parser::syntax_tree::{
    Function, Program, SrcFunction, SrcStatement, Statement,
};
use crate::compiler::parser::types::{SrcType, UnknownType};

pub mod analyzed_syntax_tree;
mod expression_parser;
mod parser_error;
pub(crate) mod pretty_print;
pub mod syntax_tree;
pub mod type_analyzer;
pub mod types;
mod struct_parser;
mod type_resolver;

fn pop_or_err(tokens: &mut TokenStack, expected: Token) -> ParseResult<SrcToken> {
    let token = tokens.pop();
    if token.value != expected {
        return Err(LocationError::msg(&format!("Expected token '{}', found token '{}'", expected, token.value), &token.location));
    }
    Ok(token.clone())
}

pub(crate) fn parse(tokens: TokenStack) -> Program {
    let program = parse_program(tokens).unwrap();
    program
}

fn parse_program(mut tokens: TokenStack) -> ParseResult<Program> {
    let mut functions = Vec::new();
    let mut struct_definitions = Vec::new();
    while tokens.peek().value != Token::EOF {
        match tokens.peek().value {
            Token::Keyword(Keyword::Struct) => {
                let struct_def = parse_struct_definition(&mut tokens).context("Failed to parse struct definition.")?;
                struct_definitions.push(struct_def);
            }
            _ => {
                let func = parse_function(&mut tokens).context("Failed to parse function.")?;
                functions.push(func);
            }
        }
    }
    Ok(Program { functions, struct_definitions })
}

fn parse_function(tokens: &mut TokenStack) -> ParseResult<SrcFunction> {
    let return_type = parse_type(tokens)?;
    let token = tokens.pop();
    let location = token.location.clone();
    let name = match &token.value {
        Token::Identifier(name) => Ok(name),
        _ => Err(LocationError::expect("Identifier", token)),
    }?
        .to_string();

    pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;

    let mut args = Vec::new();

    if tokens.peek().value != Token::Static(StaticToken::CloseParen) {
        loop {
            let location = tokens.peek().location.clone();
            let arg_type = parse_type(tokens)?;
            let arg_name = match &tokens.pop().value {
                Token::Identifier(name) => Ok(name),
                _ => Err(LocationError::msg("Expected Identifier", &location)),
            }?
                .to_string();
            args.push((arg_name, arg_type));

            if tokens.peek().value != Token::Static(StaticToken::Comma) {
                break;
            }
            tokens.pop();
        }
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;

    let expr = parse_block_expr(tokens)?;

    Ok(SrcFunction::new(
        Function {
            return_type,
            args,
            name,
            expr,
        },
        &location,
    ))
}

fn parse_statement(tokens: &mut TokenStack) -> ParseResult<SrcStatement> {
    let token = tokens.peek();
    let location = token.location.clone();
    match &token.value {
        Token::Keyword(Keyword::Return) => {
            tokens.pop();
            if tokens.peek().value == Token::Static(StaticToken::Semicolon) {
                tokens.pop();
                return Ok(SrcStatement::new(Statement::Return(None), &location));
            }
            let expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
            Ok(SrcStatement::new(Statement::Return(Some(expr)), &location))
        }
        Token::Keyword(Keyword::If) => {
            tokens.pop();
            let condition = parse_expression(tokens)?;
            let true_expr = parse_block_expr(tokens)?;
            let false_expr = if tokens.peek().value == Token::Keyword(Keyword::Else) {
                tokens.pop();
                match tokens.peek().value {
                    Token::Keyword(Keyword::If) => {
                        Some(Box::new(parse_statement(tokens)?))
                    }
                    _ => {
                        let else_expr = parse_block_expr(tokens)?;
                        Some(Box::new(SrcStatement::new(Statement::Expr(else_expr), &location)))
                    }
                }
            } else {
                None
            };
            Ok(SrcStatement::new(
                Statement::If {
                    condition,
                    true_expr,
                    false_statement: false_expr,
                },
                &location,
            ))
        }
        Token::Keyword(Keyword::For) => parse_for_loop(tokens, &location),
        Token::Keyword(Keyword::While) => parse_while_loop(tokens, &location),
        Token::Keyword(Keyword::Do) => parse_do_while_loop(tokens, &location),
        _ => {
            let offset = tokens.offset;
            let (decl_statement, propagate_err) = parse_declaration_statement(tokens, &location);

            match decl_statement {
                Ok(declaration) => {
                    pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
                    return Ok(declaration);
                }
                Err(err) if propagate_err => {
                    println!("Err {}", err);
                    return Err(err);
                }
                _ => tokens.offset = offset
            }
            let expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
            Ok(SrcStatement::new(Statement::Expr(expr), &location))
        }
    }
}

fn parse_declaration_statement(tokens: &mut TokenStack, location: &Location) -> (ParseResult<SrcStatement>, bool) {
    let var_type = match parse_type(tokens) {
        Ok(var_type) => var_type,
        Err(err) => return (Err(err), false),
    };
    let token = tokens.pop();
    let name = match &token.value {
        Token::Identifier(name) => name,
        _ => return (Err(LocationError::expect("Identifier", token)), false),
    }.to_string();

    if let Err(err) = pop_or_err(tokens, Token::Static(StaticToken::Assign)) {
        return (Err(err), false);
    }

    let value = match parse_expression(tokens) {
        Ok(value) => value,
        Err(err) => return (Err(err), true),
    };

    (Ok(SrcStatement::new(Statement::Declaration {
        var_type,
        name,
        value,
    }, location)), true)
}

fn parse_for_loop(tokens: &mut TokenStack, location: &Location) -> ParseResult<SrcStatement> {
    pop_or_err(tokens, Token::Keyword(Keyword::For))?;
    pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;
    let init = Box::new(parse_statement(tokens)?);
    let condition = parse_expression(tokens)?;
    pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
    let update = parse_expression(tokens)?;
    pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;
    let body = parse_block_expr(tokens)?;
    Ok(SrcStatement::new(
        Statement::For {
            init,
            condition,
            update,
            body,
        },
        location,
    ))
}

fn parse_while_loop(tokens: &mut TokenStack, location: &Location) -> ParseResult<SrcStatement> {
    pop_or_err(tokens, Token::Keyword(Keyword::While))?;
    let condition = parse_expression(tokens)?;
    let body = parse_block_expr(tokens)?;
    Ok(SrcStatement::new(
        Statement::While {
            condition,
            body,
            is_do_while: false,
        },
        location,
    ))
}

fn parse_do_while_loop(tokens: &mut TokenStack, location: &Location) -> ParseResult<SrcStatement> {
    pop_or_err(tokens, Token::Keyword(Keyword::Do))?;
    let body = parse_block_expr(tokens)?;
    pop_or_err(tokens, Token::Keyword(Keyword::While))?;
    let condition = parse_expression(tokens)?;
    pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
    Ok(SrcStatement::new(
        Statement::While {
            condition,
            body,
            is_do_while: true,
        },
        location,
    ))
}

fn parse_type(tokens: &mut TokenStack) -> ParseResult<SrcType> {
    let token = tokens.pop();
    let location = token.location.clone();
    match &token.value {
        Token::Identifier(str) => Ok(SrcType::new(UnknownType::Named(str.to_string()), &location)),
        Token::Static(StaticToken::Ampersand) => {
            let inner_type = Box::new(parse_type(tokens)?);
            Ok(SrcType::new(UnknownType::Pointer(inner_type), &location))
        }
        Token::Static(StaticToken::LogicalAnd) => {
            let inner_type = Box::new(parse_type(tokens)?);
            Ok(SrcType::new(UnknownType::Pointer(Box::new(SrcType::new(UnknownType::Pointer(inner_type), &location))), &location))
        }
        _ => Err(LocationError::msg(&format!("Invalid token: '{}'", token.value), &token.location)),
    }
}
