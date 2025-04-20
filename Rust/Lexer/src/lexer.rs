#![allow(dead_code)]

use std::cell::Cell;

use crate::token;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    curr: Cell<usize>,
    peek: Cell<usize>,
    char: Cell<char>,
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        let lex = Lexer {
            input,
            curr: Default::default(),
            peek: Default::default(),
            char: Default::default(),
        };
        lex.read_char();
        lex
    }

    pub fn next_token(&self) -> token::Token<'_> {
        while self.char.get().is_whitespace() {
            self.read_char();
        }

        use token::{get_identifier_map, Token, TokenType::*};
        let tok = match self.char.get() {
            '\0' => Token::new(Eof, "\0"),
            '=' => Token::new(Equal, "="),
            '-' => Token::new(Dash, "-"),
            '+' => Token::new(Plus, "+"),
            '(' => Token::new(LParen, "("),
            ')' => Token::new(RParen, ")"),
            '{' => Token::new(LBrace, "{"),
            '}' => Token::new(RBrace, "}"),
            ';' => Token::new(Semicolon, ";"),
            ':' => Token::new(Colon, ":"),
            '<' => Token::new(LessThan, "<"),
            '>' => Token::new(GreaterThan, ">"),
            'a'..='z' | 'A'..='Z' => {
                return {
                    let ident = self.read_identifier();
                    match get_identifier_map().get(ident) {
                        Some(tok_type) => Token::new(*tok_type, ident),
                        None => Token::new(Identifier, ident),
                    }
                }
            },
            '0'..='9' => return Token::new(Number, self.read_number()),
            _ => Token::new(Illegal, "ILLEGAL"),
        };
        self.read_char();
        tok
    }

    fn read_number(&self) -> &str {
        let pos = self.curr.get();
        while self.char.get().is_numeric() {
            self.read_char();
        }
        &self.input[pos..self.curr.get()]
    }

    fn read_identifier(&self) -> &str {
        let pos = self.curr.get();
        while self.char.get().is_alphabetic() || self.char.get() == '_' {
            self.read_char();
        }
        &self.input[pos..self.curr.get()]
    }

    fn read_char(&self) {
        let peek = self.peek.get();
        if peek >= self.input.len() {
            self.char.set('\0')
        } else {
            self.char.set(self.input.as_bytes()[peek].into())
        }
        self.curr.set(peek);
        self.peek.set(peek + 1);
    }
}
