use crate::lexer::{Keyword, Token};
use crate::lexer::token_stack::TokenStack;
use crate::parser::expression_parser::parse_expression;
use crate::parser::parser_error::{ParseResult, ParserError};
use crate::parser::syntax_tree::{Function, Program, Statement, Type};

mod syntax_tree;
mod parser_error;
mod expression_parser;

fn pop_or_err(tokens: &mut TokenStack, expected: Token) -> Result<(), ParserError> {
    let token = tokens.pop();
    if token.token != expected {
        return Err(ParserError::expect(expected, token));
    }
    Ok(())
}

pub(crate) fn parse(mut tokens: TokenStack) -> ParseResult<Program> {
    let mut functions = Vec::new();
    while tokens.peek().token != Token::EOF {
        let func = parse_function(&mut tokens)?;
        functions.push(func);
    }
    Ok(Program { functions })
}

fn parse_function(tokens: &mut TokenStack) -> ParseResult<Function> {
    let return_type = parse_type(tokens)?;
    let token = tokens.pop();
    let name = match &token.token {
        Token::Identifier(name) => Ok(name),
        _ => Err(ParserError::expect("Identifier", token)),
    }?.to_string();

    pop_or_err(tokens, Token::OpenParen)?;

    let mut args = Vec::new();

    if tokens.peek().token != Token::CloseParen {
        loop {
            let arg_type = parse_type(tokens)?;
            if arg_type.is_none() {
                return Err(ParserError::expect("Type", tokens.peek()));
            }
            let arg_name = match &tokens.pop().token {
                Token::Identifier(name) => Ok(name),
                _ => Err(ParserError::expect("Identifier", tokens.peek())),
            }?.to_string();
            args.push((arg_name, arg_type.unwrap()));

            if tokens.peek().token != Token::Comma {
                break;
            }
            tokens.pop();
        }
    }
    pop_or_err(tokens, Token::CloseParen)?;

    pop_or_err(tokens, Token::OpenBrace)?;

    let mut statements = Vec::new();

    while tokens.peek().token != Token::CloseBrace {
        let statement = parse_statement(tokens)?;
        statements.push(statement);
    }
    tokens.pop();

    Ok(Function {
        return_type,
        args,
        name,
        statements,
    })
}

fn parse_statement(tokens: &mut TokenStack) -> ParseResult<Statement> {
    let token = tokens.peek();
    match &token.token {
        Token::Keyword(Keyword::Return) => {
            tokens.pop();
            if tokens.peek().token == Token::Semicolon {
                tokens.pop();
                return Ok(Statement::Return(None));
            }
            let expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::Semicolon)?;
            Ok(Statement::Return(Some(expr)))
        }
        _ => {
            let offset = tokens.offset;
            match parse_declaration_statement(tokens) {
                Ok(declaration) => {
                    pop_or_err(tokens, Token::Semicolon)?;
                    return Ok(declaration);
                }
                Err(_) => tokens.offset = offset
            }
            let expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::Semicolon)?;
            Ok(Statement::Expr(expr))
        }
    }
}

fn parse_declaration_statement(tokens: &mut TokenStack) -> ParseResult<Statement> {
    let var_type = match parse_type(tokens)? {
        Some(var_type) => var_type,
        None => return Err(ParserError::expect("Type", tokens.peek())),
    };

    let token = tokens.pop();
    let name = match &token.token {
        Token::Identifier(name) => Ok(name),
        _ => Err(ParserError::expect("Identifier", &token)),
    }?.to_string();

    let token = tokens.peek();
    if token.token != Token::Assign {
        return Ok(Statement::Declaration { var_type, name, value: None });
    }
    tokens.pop();

    let value = Some(parse_expression(tokens)?);

    Ok(Statement::Declaration { var_type, name, value })
}

fn parse_type(tokens: &mut TokenStack) -> ParseResult<Option<Type>> {
    let token = tokens.pop();
    if let Token::Identifier(str) = &token.token {
        match str.as_str() {
            "void" => Ok(None),
            "int" => Ok(Some(Type::Int)),
            "long" => Ok(Some(Type::Long)),
            "string" => Ok(Some(Type::String)),
            _ => Err(ParserError {
                message: format!("Unknown type: {}", str),
                line: token.line,
                column: token.column,
            }),
        }
    } else {
        Err(ParserError::expect("Type", &token))
    }
}