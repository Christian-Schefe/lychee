use crate::compiler::lexer::location::Location;
use crate::compiler::lexer::SrcToken;

pub(crate) struct TokenStack {
    stack: Vec<SrcToken>,
    pub(crate) offset: usize,
    size: usize,
}

impl TokenStack {
    pub fn new(tokens: Vec<SrcToken>) -> Self {
        TokenStack {
            size: tokens.len(),
            stack: tokens,
            offset: 0,
        }
    }

    pub fn peek(&self) -> &SrcToken {
        self.stack.get(self.offset).unwrap()
    }

    pub fn shift(&mut self) -> &SrcToken {
        if self.offset < self.size {
            let token = &self.stack[self.offset];
            self.offset += 1;
            token
        } else {
            panic!("Tried to shift beyond token stack");
        }
    }

    pub fn reverse_shift(&mut self) {
        if self.offset > 0 {
            self.offset -= 1;
        } else {
            panic!("Tried to reverse shift beyond token stack");
        }
    }

    pub fn replace(&mut self, token: SrcToken) {
        self.stack[self.offset] = token;
    }

    pub fn insert(&mut self, token: SrcToken) {
        self.stack.insert(self.offset, token);
        self.size += 1;
    }

    pub fn location(&self) -> &Location {
        &self.peek().location
    }
}
