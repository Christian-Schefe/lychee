pub mod token_stack;
pub mod token;

use std::string::String;
use std::path::PathBuf;
use crate::lexer::token::{StaticToken, Token, STATIC_TOKEN_MAP};

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

pub trait HasLocation {
    fn location(&self) -> &Location;
}

#[derive(Debug, Clone)]
pub struct SrcToken {
    pub(crate) token: Token,
    pub(crate) location: Location,
}

impl HasLocation for SrcToken {
    fn location(&self) -> &Location {
        &self.location
    }
}

pub fn lex(input_path: PathBuf) -> Vec<SrcToken> {
    let input: Vec<char> = std::fs::read_to_string(input_path).unwrap().chars().collect();
    let mut offset = 0;
    let mut tokens = Vec::new();
    let mut line = 1;
    let mut column = 1;

    while offset < input.len() {
        if input[offset].is_whitespace() {
            if input[offset] == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
            offset += 1;
            continue;
        }
        let (token, new_offset) = if input[offset] == '"' {
            read_string(&input, offset)
        } else {
            read_token(&input, offset)
        };
        tokens.push(SrcToken { token, location: Location { line, column } });
        column += new_offset - offset;
        offset = new_offset;
    }
    tokens.push(SrcToken { token: Token::EOF, location: Location { line, column } });

    tokens
}

fn read_string(input: &Vec<char>, offset: usize) -> (Token, usize) {
    let mut string = String::new();
    let mut i = offset + 1;

    while i < input.len() {
        if input[i] == '"' {
            return (Token::String(string), i + 1);
        } else if input[i] == '\\' {
            i += 1;
            if i >= input.len() {
                panic!("Unterminated string");
            }
            match input[i] {
                'n' => string.push('\n'),
                'r' => string.push('\r'),
                't' => string.push('\t'),
                '\\' => string.push('\\'),
                '"' => string.push('"'),
                _ => panic!("Invalid escape sequence"),
            }
        } else {
            string.push(input[i]);
        }
        i += 1;
    }

    panic!("Unterminated string");
}

fn read_token(input: &Vec<char>, offset: usize) -> (Token, usize) {
    for len in (1..=StaticToken::MAX_LENGTH).rev() {
        if offset + len > input.len() {
            continue;
        }
        let substr: String = input[offset..offset + len].iter().collect();
        if let Some(token) = STATIC_TOKEN_MAP.get(&substr) {
            return (Token::Static(token.clone()), offset + len);
        }
    }

    let mut substr = String::new();
    let mut last_valid = None;

    for i in offset..input.len() {
        substr.push(input[i]);
        if let Some(token) = Token::token_from_str(&substr) {
            last_valid = Some((token, i + 1));
        } else {
            break;
        }
    }

    if let Some((token, new_offset)) = last_valid {
        (token, new_offset)
    } else {
        panic!("Invalid token: {}", substr);
    }
}