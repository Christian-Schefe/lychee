use lazy_static::lazy_static;
use std::collections::HashMap;
use crate::compiler::parser::syntax_tree::Literal;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StaticToken {
    Semicolon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Colon,
    QuestionMark,
    Plus,
    Minus,
    Asterisk,
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
    Equals,
    NotEquals,
    Dot,
    Increment,
    Decrement,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    XorAssign,
    AndAssign,
    OrAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    LogicalAndAssign,
    LogicalOrAssign,
}

impl StaticToken {
    pub fn get_str(&self) -> String {
        match self {
            StaticToken::Semicolon => ";".to_string(),
            StaticToken::OpenParen => "(".to_string(),
            StaticToken::CloseParen => ")".to_string(),
            StaticToken::OpenBrace => "{".to_string(),
            StaticToken::CloseBrace => "}".to_string(),
            StaticToken::OpenBracket => "[".to_string(),
            StaticToken::CloseBracket => "]".to_string(),
            StaticToken::Comma => ",".to_string(),
            StaticToken::Colon => ":".to_string(),
            StaticToken::QuestionMark => "?".to_string(),
            StaticToken::Plus => "+".to_string(),
            StaticToken::Minus => "-".to_string(),
            StaticToken::Asterisk => "*".to_string(),
            StaticToken::Slash => "/".to_string(),
            StaticToken::Percent => "%".to_string(),
            StaticToken::Caret => "^".to_string(),
            StaticToken::Ampersand => "&".to_string(),
            StaticToken::Pipe => "|".to_string(),
            StaticToken::LogicalOr => "||".to_string(),
            StaticToken::LogicalAnd => "&&".to_string(),
            StaticToken::ShiftLeft => "<<".to_string(),
            StaticToken::ShiftRight => ">>".to_string(),
            StaticToken::Tilde => "~".to_string(),
            StaticToken::ExclamationMark => "!".to_string(),
            StaticToken::LessThan => "<".to_string(),
            StaticToken::GreaterThan => ">".to_string(),
            StaticToken::LessThanOrEqual => "<=".to_string(),
            StaticToken::GreaterThanOrEqual => ">=".to_string(),
            StaticToken::Equals => "==".to_string(),
            StaticToken::NotEquals => "!=".to_string(),
            StaticToken::Dot => ".".to_string(),
            StaticToken::Increment => "++".to_string(),
            StaticToken::Decrement => "--".to_string(),
            StaticToken::Assign => "=".to_string(),
            StaticToken::AddAssign => "+=".to_string(),
            StaticToken::SubAssign => "-=".to_string(),
            StaticToken::MulAssign => "*=".to_string(),
            StaticToken::DivAssign => "/=".to_string(),
            StaticToken::ModAssign => "%=".to_string(),
            StaticToken::XorAssign => "^=".to_string(),
            StaticToken::AndAssign => "&=".to_string(),
            StaticToken::OrAssign => "|=".to_string(),
            StaticToken::ShiftLeftAssign => "<<=".to_string(),
            StaticToken::ShiftRightAssign => ">>=".to_string(),
            StaticToken::LogicalAndAssign => "&&=".to_string(),
            StaticToken::LogicalOrAssign => "||=".to_string(),
        }
    }

    pub const VALUES: [StaticToken; 46] = [
        StaticToken::Semicolon,
        StaticToken::OpenParen,
        StaticToken::CloseParen,
        StaticToken::OpenBrace,
        StaticToken::CloseBrace,
        StaticToken::OpenBracket,
        StaticToken::CloseBracket,
        StaticToken::Comma,
        StaticToken::Colon,
        StaticToken::QuestionMark,
        StaticToken::Plus,
        StaticToken::Minus,
        StaticToken::Asterisk,
        StaticToken::Slash,
        StaticToken::Percent,
        StaticToken::Caret,
        StaticToken::Ampersand,
        StaticToken::Pipe,
        StaticToken::LogicalOr,
        StaticToken::LogicalAnd,
        StaticToken::ShiftLeft,
        StaticToken::ShiftRight,
        StaticToken::Tilde,
        StaticToken::ExclamationMark,
        StaticToken::LessThan,
        StaticToken::GreaterThan,
        StaticToken::LessThanOrEqual,
        StaticToken::GreaterThanOrEqual,
        StaticToken::Equals,
        StaticToken::NotEquals,
        StaticToken::Dot,
        StaticToken::Increment,
        StaticToken::Decrement,
        StaticToken::Assign,
        StaticToken::AddAssign,
        StaticToken::SubAssign,
        StaticToken::MulAssign,
        StaticToken::DivAssign,
        StaticToken::ModAssign,
        StaticToken::XorAssign,
        StaticToken::AndAssign,
        StaticToken::OrAssign,
        StaticToken::ShiftLeftAssign,
        StaticToken::ShiftRightAssign,
        StaticToken::LogicalAndAssign,
        StaticToken::LogicalOrAssign,
    ];

    pub const MAX_LENGTH: usize = 3;
}

lazy_static! {
    pub static ref STATIC_TOKEN_MAP: HashMap<String, StaticToken> = StaticToken::VALUES
        .iter()
        .map(|token| (token.get_str(), token.clone()))
        .collect();
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Static(StaticToken),
    Identifier(String),
    Literal(Literal),
    String(String),
    Keyword(Keyword),
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Return,
    If,
    Else,
    For,
    While,
    Do,
}

impl Keyword {
    pub fn from_str(str: &str) -> Option<Self> {
        match str {
            "return" => Some(Keyword::Return),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "for" => Some(Keyword::For),
            "while" => Some(Keyword::While),
            "do" => Some(Keyword::Do),
            _ => None,
        }
    }
}

impl Token {
    pub(crate) fn token_from_str(str: &str) -> Result<Self, bool> {
        if let Some(keyword) = Keyword::from_str(str) {
            Ok(Token::Keyword(keyword))
        } else if str == "true" {
            Ok(Token::Literal(Literal::Bool(true)))
        } else if str == "false" {
            Ok(Token::Literal(Literal::Bool(false)))
        } else if str.chars().all(|c| c.is_numeric()) {
            str.parse::<i32>().map(|x| Token::Literal(Literal::Int(x))).map_err(|_| false)
        } else if str.len() >= 2 && str.chars().enumerate().all(|(i, c)| if i == str.len() - 1 { c == 'b' } else { c.is_numeric() }) {
            str[..str.len() - 1].parse::<i8>().map(|x| Token::Literal(Literal::Byte(x))).map_err(|_| true)
        } else if str.len() >= 2 && str.chars().enumerate().all(|(i, c)| if i == str.len() - 1 { c == 's' } else { c.is_numeric() }) {
            str[..str.len() - 1].parse::<i16>().map(|x| Token::Literal(Literal::Short(x))).map_err(|_| true)
        } else if str.len() >= 2 && str.chars().enumerate().all(|(i, c)| if i == str.len() - 1 { c == 'l' } else { c.is_numeric() }) {
            str[..str.len() - 1].parse::<i64>().map(|x| Token::Literal(Literal::Long(x))).map_err(|_| true)
        } else if str.chars().enumerate().all(|(i, c)| if i == 0 { c.is_alphabetic() } else { c.is_alphanumeric() } || c == '_') {
            Ok(Token::Identifier(str.to_string()))
        } else {
            Err(true)
        }
    }
}
