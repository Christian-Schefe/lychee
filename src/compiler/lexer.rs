pub mod lexer_error;
pub mod location;
pub mod token;
pub mod token_stack;

use crate::compiler::lexer::lexer_error::{LexResult, LocationError};
use crate::compiler::lexer::location::{Location, Src};
use crate::compiler::lexer::token::{Literal, StaticToken, Token, STATIC_TOKEN_MAP};
use std::path::PathBuf;
use std::string::String;

pub type SrcToken = Src<Token>;

pub fn lex(input_path: &PathBuf) -> LexResult<Vec<SrcToken>> {
    let input: Vec<char> = std::fs::read_to_string(input_path)?.chars().collect();
    let mut offset = 0;
    let mut tokens = Vec::new();
    let mut location = Location::new(input_path.clone());

    while offset < input.len() {
        if input[offset] == '/' && offset + 1 < input.len() && input[offset + 1] == '/' {
            while offset < input.len() && input[offset] != '\n' {
                offset += 1;
            }
            continue;
        }
        let (token, new_offset) = match input[offset] {
            c if c.is_whitespace() => {
                if input[offset] == '\n' {
                    location.advance_line();
                } else {
                    location.advance_column(1);
                }
                offset += 1;
                continue;
            }
            '"' => read_string(&input, &location, offset, '"')?,
            '\'' => read_char(&input, &location, offset)?,
            _ => read_token(&input, &location, offset)?,
        };
        tokens.push(SrcToken {
            value: token,
            location: location.clone(),
        });
        location.advance_column(new_offset - offset);
        offset = new_offset;
    }
    tokens.push(SrcToken {
        value: Token::EOF,
        location,
    });

    Ok(tokens)
}

fn read_string(
    input: &Vec<char>,
    location: &Location,
    offset: usize,
    quote_char: char,
) -> LexResult<(Token, usize)> {
    let mut string = String::new();
    let mut i = offset + 1;

    while i < input.len() {
        if input[i] == quote_char {
            return Ok((Token::Literal(Literal::String(string)), i + 1));
        } else if input[i] == '\\' {
            i += 1;
            if i >= input.len() {
                return Err(LocationError::new(
                    "Unterminated string.".to_string(),
                    location.clone(),
                ))?;
            }
            match input[i] {
                'n' => string.push('\n'),
                'r' => string.push('\r'),
                't' => string.push('\t'),
                '\\' => string.push('\\'),
                '"' => string.push('"'),
                '\'' => string.push('\''),
                '0' => string.push('\0'),
                _ => {
                    return Err(LocationError::new(
                        "Invalid escape sequence.".to_string(),
                        location.clone(),
                    ))?
                }
            }
        } else {
            string.push(input[i]);
        }
        i += 1;
    }

    Err(LocationError::new(
        "Unterminated string.".to_string(),
        location.clone(),
    ))?
}

fn read_char(input: &Vec<char>, location: &Location, offset: usize) -> LexResult<(Token, usize)> {
    let (c, new_offset) = read_string(input, location, offset, '\'')?;
    let str = match c {
        Token::Literal(Literal::String(s)) => s,
        _ => {
            return Err(LocationError::new(
                "Invalid char literal.".to_string(),
                location.clone(),
            ))?
        }
    };
    if str.len() != 1 {
        return Err(LocationError::new(
            "Invalid char literal.".to_string(),
            location.clone(),
        ))?;
    }
    let char = str.chars().next().unwrap();
    if char.len_utf8() != 1 {
        Err(LocationError::new(
            "Invalid char literal.".to_string(),
            location.clone(),
        ))?
    } else {
        Ok((Token::Literal(Literal::Char(char)), new_offset))
    }
}

fn read_token(input: &Vec<char>, location: &Location, offset: usize) -> LexResult<(Token, usize)> {
    for len in (1..=StaticToken::MAX_LENGTH).rev() {
        if offset + len > input.len() {
            continue;
        }
        let substr: String = input[offset..offset + len].iter().collect();
        if let Some(token) = STATIC_TOKEN_MAP.get(&substr) {
            return Ok((Token::Static(token.clone()), offset + len));
        }
    }

    let substr = input[offset..]
        .iter()
        .take_while(|&c| c.is_ascii_alphanumeric() || *c == '_')
        .collect::<String>();
    let token = Token::token_from_str(&substr).ok_or(LocationError::new(
        format!("Invalid token: '{substr}'"),
        location.clone(),
    ))?;

    match substr.len() {
        0 => Err(LocationError::new(
            format!("Invalid token: '{}'", input[offset]),
            location.clone(),
        ))?,
        len => Ok((token, offset + len)),
    }
}
