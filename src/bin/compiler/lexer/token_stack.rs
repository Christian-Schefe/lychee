use crate::lexer::{SrcToken, Token};

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

    pub fn is_empty(&self) -> bool {
        self.offset == self.size
    }

    pub fn peek(&self) -> &SrcToken {
        self.stack.get(self.offset).unwrap()
    }

    pub fn pop(&mut self) -> &SrcToken {
        if self.offset < self.size {
            let token = &self.stack[self.offset];
            self.offset += 1;
            token
        } else {
            panic!("Tried to pop from empty token stack");
        }
    }

    pub fn pop_if<F>(&mut self, f: F) -> Option<SrcToken>
    where
        F: Fn(&Token) -> bool,
    {
        if self.offset < self.size && f(&self.stack[self.offset].token) {
            let token = self.stack[self.offset].clone();
            self.offset += 1;
            Some(token)
        } else {
            None
        }
    }
}