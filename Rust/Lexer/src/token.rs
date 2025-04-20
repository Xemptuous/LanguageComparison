#![allow(dead_code)]

use std::{
    collections::HashMap,
    sync::{Mutex, MutexGuard, OnceLock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Eof,
    Illegal,

    Let,
    Function,
    If,
    Else,
    Return,

    Identifier,
    Number,
    Boolean,

    Int,
    Bool,

    Asterisk,    // *
    Slash,       // /
    Dash,        // -
    Plus,        // +
    Equal,       // =
    LessThan,    // <
    GreaterThan, // >
    Semicolon,   // ;
    Colon,       // :
    LParen,      // (
    RParen,      // )
    LBrace,      // {
    RBrace,      // }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub literal: &'a str,
    pub token_type: TokenType,
}

impl Token<'_> {
    pub fn new(token_type: TokenType, literal: &str) -> Token {
        Token { literal, token_type }
    }
}

pub fn get_identifier_map() -> MutexGuard<'static, HashMap<&'static str, TokenType>> {
    static IDENTIFIER_MAP: OnceLock<Mutex<HashMap<&str, TokenType>>> = OnceLock::new();
    IDENTIFIER_MAP
        .get_or_init(|| {
            use TokenType::*;
            let mut m = HashMap::new();
            m.insert("let", Let);
            m.insert("fn", Function);
            m.insert("if", If);
            m.insert("else", Else);
            m.insert("return", Return);

            m.insert("true", Boolean);
            m.insert("false", Boolean);

            m.insert("bool", Bool);
            m.insert("int", Int);
            Mutex::new(m)
        })
        .lock()
        .expect("Let's hope the lock isn't poisoned")
}
