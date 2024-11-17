use crate::compiler::lexer::token::{Keyword, StaticToken, Token};
use crate::compiler::lexer::token_stack::TokenStack;
use crate::compiler::parser::expression_parser::{parse_block_expr, parse_expression};
use crate::compiler::parser::parser_error::{LocationError, ParseResult};
use crate::compiler::parser::syntax_tree::{Function, Program, SrcFunction, SrcStatement, Statement, Type};

pub mod syntax_tree;
mod parser_error;
mod expression_parser;
pub(crate) mod pretty_print;
pub mod type_analyzer;
pub mod analyzed_syntax_tree;

fn pop_or_err(tokens: &mut TokenStack, expected: Token) -> Result<(), LocationError> {
    let token = tokens.pop();
    if token.token != expected {
        return Err(LocationError::expect(expected, token));
    }
    Ok(())
}

pub(crate) fn parse(tokens: TokenStack) -> Program {
    let program = parse_program(tokens).unwrap();
    program
}

fn parse_program(mut tokens: TokenStack) -> ParseResult<Program> {
    let mut functions = Vec::new();
    while tokens.peek().token != Token::EOF {
        let func = parse_function(&mut tokens)?;
        functions.push(func);
    }
    Ok(Program { functions })
}

fn parse_function(tokens: &mut TokenStack) -> ParseResult<SrcFunction> {
    let return_type = parse_type(tokens)?;
    let token = tokens.pop();
    let location = token.location.clone();
    let name = match &token.token {
        Token::Identifier(name) => Ok(name),
        _ => Err(LocationError::expect("Identifier", token)),
    }?.to_string();

    pop_or_err(tokens, Token::Static(StaticToken::OpenParen))?;

    let mut args = Vec::new();

    if tokens.peek().token != Token::Static(StaticToken::CloseParen) {
        loop {
            let arg_type = parse_type(tokens)?;
            if arg_type.is_none() {
                return Err(LocationError::expect("Type", tokens.peek()));
            }
            let arg_name = match &tokens.pop().token {
                Token::Identifier(name) => Ok(name),
                _ => Err(LocationError::expect("Identifier", tokens.peek())),
            }?.to_string();
            args.push((arg_name, arg_type.unwrap()));

            if tokens.peek().token != Token::Static(StaticToken::Comma) {
                break;
            }
            tokens.pop();
        }
    }
    pop_or_err(tokens, Token::Static(StaticToken::CloseParen))?;

    let expr = parse_expression(tokens)?;

    Ok(SrcFunction::new(Function {
        return_type,
        args,
        name,
        expr,
    }, &location))
}

fn parse_statement(tokens: &mut TokenStack) -> ParseResult<SrcStatement> {
    let token = tokens.peek();
    let location = token.location.clone();
    match &token.token {
        Token::Keyword(Keyword::Return) => {
            tokens.pop();
            if tokens.peek().token == Token::Static(StaticToken::Semicolon) {
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
            let false_expr = if tokens.peek().token == Token::Keyword(Keyword::Else) {
                tokens.pop();
                Some(parse_block_expr(tokens)?)
            } else {
                None
            };
            Ok(SrcStatement::new(Statement::If { condition, true_expr, false_expr }, &location))
        }
        _ => {
            let offset = tokens.offset;
            if let Ok(declaration) = parse_declaration_statement(tokens) {
                pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
                return Ok(SrcStatement::new(declaration, &location));
            }
            tokens.offset = offset;
            let expr = parse_expression(tokens)?;
            pop_or_err(tokens, Token::Static(StaticToken::Semicolon))?;
            Ok(SrcStatement::new(Statement::Expr(expr), &location))
        }
    }
}

fn parse_declaration_statement(tokens: &mut TokenStack) -> ParseResult<Statement> {
    let var_type = match parse_type(tokens)? {
        Some(var_type) => var_type,
        None => return Err(LocationError::expect("Type", tokens.peek())),
    };

    let token = tokens.pop();
    let name = match &token.token {
        Token::Identifier(name) => Ok(name),
        _ => Err(LocationError::expect("Identifier", token)),
    }?.to_string();

    let token = tokens.peek();
    if token.token != Token::Static(StaticToken::Assign) {
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
            "int" => Ok(Some(Type::Int)),
            "long" => Ok(Some(Type::Long)),
            "bool" => Ok(Some(Type::Bool)),
            "string" => Ok(Some(Type::String)),
            _ => Err(LocationError {
                message: format!("Unknown type: {}", str),
                location: token.location.clone(),
            }),
        }
    } else if let Token::Keyword(Keyword::Void) = token.token {
        Ok(None)
    } else {
        Err(LocationError::expect("Type", token))
    }
}