#![allow(dead_code)]

use std::cell::Cell;

use crate::token::{self, get_double_token_map, TokenType};

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
        lex.advance();
        lex
    }

    pub fn next_token(&self) -> token::Token<'_> {
        while self.char.get().is_whitespace() {
            self.advance();
        }

        if let Some(ds) = self.input.get(self.curr.get()..self.peek.get() + 1) {
            if ds == "//" {
                return Token::new(Comment, self.read_comment());
            }
            if let Some(tok_type) = get_double_token_map().get(ds) {
                self.advance();
                self.advance();
                return Token::new(*tok_type, ds);
            }
        }

        use token::{get_identifier_map, Token, TokenType::*};
        let tok = match self.char.get() {
            '\0' => Token::new(Eof, "\0"),
            '!' => Token::new(Exclamation, "!"),
            '@' => Token::new(At, "@"),
            '#' => Token::new(Hashtag, "#"),
            '$' => Token::new(Dollar, "$"),
            '%' => Token::new(Percent, "%"),
            '^' => Token::new(Caret, "^"),
            '&' => Token::new(Ampersand, "&"),
            '*' => Token::new(Asterisk, "*"),
            '(' => Token::new(Lparen, "("),
            ')' => Token::new(Rparen, ")"),
            '-' => Token::new(Minus, "-"),
            '_' => Token::new(Underscore, "_"),
            '+' => Token::new(Plus, "+"),
            '=' => Token::new(Assign, "="),
            '[' => Token::new(Lbracket, "["),
            ']' => Token::new(Rbracket, "]"),
            '{' => Token::new(Lbrace, "{"),
            '}' => Token::new(Rbrace, "}"),
            ';' => Token::new(Semicolon, ";"),
            ':' => Token::new(Colon, ":"),
            '\'' => Token::new(Char, self.read_char()),
            '"' => Token::new(String, self.read_string()),
            ',' => Token::new(Comma, ","),
            '.' => Token::new(Period, "."),
            '<' => Token::new(Lessthan, "<"),
            '>' => Token::new(Greaterthan, ">"),
            '/' => Token::new(Slash, "/"),
            '?' => Token::new(Question, "?"),
            '\\' => Token::new(Backslash, "\\"),
            '|' => Token::new(Pipe, "|"),
            'a'..='z' | 'A'..='Z' => {
                return {
                    let ident = self.read_identifier();
                    match get_identifier_map().get(ident) {
                        Some(tok_type) => Token::new(*tok_type, ident),
                        None => Token::new(Identifier, ident),
                    }
                }
            },
            '0'..='9' => {
                return {
                    let (ttype, lit) = self.read_number();
                    Token::new(ttype, lit)
                }
            },
            _ => Token::new(Illegal, "ILLEGAL"),
        };
        self.advance();
        tok
    }

    fn read_number(&self) -> (TokenType, &str) {
        let pos = self.curr.get();
        let mut is_float = false;
        while self.char.get().is_numeric() || self.char.get() == '.' {
            if self.char.get() == '.' {
                if is_float {
                    self.advance();
                    return (TokenType::Illegal, "ILLEGAL");
                }
                is_float = true;
            }
            self.advance();
        }
        if is_float {
            (TokenType::Float, &self.input[pos..self.curr.get()])
        } else {
            (TokenType::Number, &self.input[pos..self.curr.get()])
        }
    }

    fn read_identifier(&self) -> &str {
        let pos = self.curr.get();
        while self.char.get().is_alphanumeric() || self.char.get() == '_' {
            self.advance();
        }
        &self.input[pos..self.curr.get()]
    }

    fn read_string(&self) -> &str {
        self.advance();
        let pos = self.curr.get();
        while self.char.get() != '"' {
            self.advance();
        }
        &self.input[pos..self.curr.get()]
    }

    fn read_char(&self) -> &str {
        self.advance();
        let pos = self.curr.get();
        while self.char.get() != '\'' {
            self.advance();
        }
        &self.input[pos..self.curr.get()]
    }

    fn read_comment(&self) -> &str {
        let pos = self.curr.get();
        while self.char.get() != '\n' && self.char.get() != '\r' {
            self.advance();
        }
        &self.input[pos..self.curr.get()]
    }

    fn advance(&self) {
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
