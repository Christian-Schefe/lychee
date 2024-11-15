pub mod token_stack;

use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Semicolon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Colon,
    QuestionMark,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Ampersand,
    Pipe,
    LogicalOr,
    LogicalAnd,
    ShiftLeft,
    ShiftRight,
    Tilde,
    ExclamationMark,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Dot,
    Identifier(String),
    Integer(i64),
    String(String),
    Increment,
    Decrement,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    CaretAssign,
    AmpersandAssign,
    PipeAssign,
    LeftShiftAssign,
    RightShiftAssign,
    Keyword(Keyword),
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Return
}

impl Token {
    fn is_final(&self) -> bool {
        match self {
            Token::Semicolon => true,
            Token::OpenParen => true,
            Token::CloseParen => true,
            Token::OpenBrace => true,
            Token::CloseBrace => true,
            Token::Comma => true,
            Token::Colon => true,
            Token::QuestionMark => true,
            Token::Tilde => true,
            Token::LogicalOr => true,
            Token::LogicalAnd => true,
            Token::Equal => true,
            Token::NotEqual => true,
            Token::LessThanOrEqual => true,
            Token::GreaterThanOrEqual => true,
            Token::Dot => true,
            Token::Increment => true,
            Token::Decrement => true,
            Token::PlusAssign => true,
            Token::MinusAssign => true,
            Token::StarAssign => true,
            Token::SlashAssign => true,
            Token::PercentAssign => true,
            Token::CaretAssign => true,
            Token::AmpersandAssign => true,
            Token::PipeAssign => true,
            Token::LeftShiftAssign => true,
            Token::RightShiftAssign => true,
            _ => false,
        }
    }

    fn from_str(str: &str) -> Option<Self> {
        if let Some(token) = match str {
            ";" => Some(Token::Semicolon),
            "(" => Some(Token::OpenParen),
            ")" => Some(Token::CloseParen),
            "{" => Some(Token::OpenBrace),
            "}" => Some(Token::CloseBrace),
            "," => Some(Token::Comma),
            ":" => Some(Token::Colon),
            "?" => Some(Token::QuestionMark),
            "+" => Some(Token::Plus),
            "-" => Some(Token::Minus),
            "*" => Some(Token::Star),
            "/" => Some(Token::Slash),
            "%" => Some(Token::Percent),
            "^" => Some(Token::Caret),
            "&" => Some(Token::Ampersand),
            "|" => Some(Token::Pipe),
            "<<" => Some(Token::ShiftLeft),
            ">>" => Some(Token::ShiftRight),
            "~" => Some(Token::Tilde),
            "||" => Some(Token::LogicalOr),
            "&&" => Some(Token::LogicalAnd),
            "!" => Some(Token::ExclamationMark),
            "<" => Some(Token::LessThan),
            ">" => Some(Token::GreaterThan),
            "<=" => Some(Token::LessThanOrEqual),
            ">=" => Some(Token::GreaterThanOrEqual),
            "==" => Some(Token::Equal),
            "=" => Some(Token::Assign),
            "!=" => Some(Token::NotEqual),
            "." => Some(Token::Dot),
            "++" => Some(Token::Increment),
            "--" => Some(Token::Decrement),
            "+=" => Some(Token::PlusAssign),
            "-=" => Some(Token::MinusAssign),
            "*=" => Some(Token::StarAssign),
            "/=" => Some(Token::SlashAssign),
            "%=" => Some(Token::PercentAssign),
            "^=" => Some(Token::CaretAssign),
            "&=" => Some(Token::AmpersandAssign),
            "|=" => Some(Token::PipeAssign),
            "<<=" => Some(Token::LeftShiftAssign),
            ">>=" => Some(Token::RightShiftAssign),
            "return" => Some(Token::Keyword(Keyword::Return)),
            _ => None,
        } {
            Some(token)
        } else if let Some(token) = Token::integer_from_str(str) {
            Some(token)
        } else if let Some(token) = Token::identifier_from_str(str) {
            Some(token)
        } else {
            None
        }
    }

    fn integer_from_str(str: &str) -> Option<Self> {
        match str.parse::<i64>() {
            Ok(int) => Some(Token::Integer(int)),
            Err(_) => None,
        }
    }

    fn identifier_from_str(str: &str) -> Option<Self> {
        let mut chars = str.chars();
        if let Some(first_char) = chars.next() {
            if first_char.is_alphabetic() && chars.all(|c| c.is_alphanumeric() || c == '_') {
                Some(Token::Identifier(str.to_string()))
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct SrcToken {
    pub(crate) token: Token,
    pub(crate) line: usize,
    pub(crate) column: usize,
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
        tokens.push(SrcToken { token, line, column });
        column += new_offset - offset;
        offset = new_offset;
    }
    tokens.push(SrcToken { token: Token::EOF, line, column });

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
    let mut last_valid_token = None;

    for i in offset..input.len() {
        let substr: String = input[offset..=i].iter().collect();
        if let Some(token) = Token::from_str(&substr) {
            if token.is_final() {
                return (token, i + 1);
            } else {
                last_valid_token = Some((token, i + 1));
            }
        }
    }

    if let Some((token, j)) = last_valid_token {
        return (token, j);
    }

    panic!("Invalid token");
}