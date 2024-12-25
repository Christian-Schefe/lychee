use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::Display;

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
    DoubleColon,
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
            StaticToken::DoubleColon => "::".to_string(),
        }
    }

    pub const VALUES: [StaticToken; 47] = [
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
        StaticToken::DoubleColon,
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
    Loop,
    Do,
    Sizeof,
    Struct,
    As,
    New,
    Continue,
    Break,
    Let,
    Var,
    Module,
    Import,
    Alias,
    Enum,
}

impl Keyword {
    pub fn from_str(str: &str) -> Option<Self> {
        match str {
            "return" => Some(Keyword::Return),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "for" => Some(Keyword::For),
            "while" => Some(Keyword::While),
            "loop" => Some(Keyword::Loop),
            "do" => Some(Keyword::Do),
            "sizeof" => Some(Keyword::Sizeof),
            "struct" => Some(Keyword::Struct),
            "as" => Some(Keyword::As),
            "new" => Some(Keyword::New),
            "continue" => Some(Keyword::Continue),
            "break" => Some(Keyword::Break),
            "let" => Some(Keyword::Let),
            "var" => Some(Keyword::Var),
            "module" => Some(Keyword::Module),
            "import" => Some(Keyword::Import),
            "alias" => Some(Keyword::Alias),
            "enum" => Some(Keyword::Enum),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    Bool(bool),
    Char(char),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(x) => write!(f, "{}", x),
            Literal::Bool(x) => write!(f, "{}", x),
            Literal::Char(x) => write!(f, "'{}'", x),
            Literal::String(x) => write!(f, "\"{}\"", x),
        }
    }
}

impl Token {
    pub(crate) fn token_from_str(str: &str) -> Option<Self> {
        if let Some(keyword) = Keyword::from_str(str) {
            Some(Token::Keyword(keyword))
        } else if str == "true" {
            Some(Token::Literal(Literal::Bool(true)))
        } else if str == "false" {
            Some(Token::Literal(Literal::Bool(false)))
        } else if str.chars().all(|c| c.is_numeric()) {
            str.parse::<i64>().map(|x| Token::Literal(Literal::Integer(x))).ok()
        } else if str.starts_with("0x") {
            i64::from_str_radix(&str[2..], 16).map(|x| Token::Literal(Literal::Integer(x))).ok()
        } else if str.starts_with("0b") {
            i64::from_str_radix(&str[2..], 2).map(|x| Token::Literal(Literal::Integer(x))).ok()
        } else if str.chars().enumerate().all(|(i, c)| if i == 0 { c.is_alphabetic() } else { c.is_alphanumeric() } || c == '_') {
            Some(Token::Identifier(str.to_string()))
        } else {
            None
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Static(token) => write!(f, "{}", token.get_str()),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::Literal(literal) => write!(f, "{:?}", literal),
            Token::Keyword(keyword) => write!(f, "{:?}", keyword),
            Token::EOF => write!(f, "EOF"),
        }
    }
}
