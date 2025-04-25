#![allow(dead_code)]

use std::{
    collections::HashMap,
    sync::{Mutex, MutexGuard, OnceLock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Eof,
    Illegal,

    // Core
    Let,
    Const,
    Struct,
    Function,
    If,
    Else,
    Switch,
    Case,
    Break,
    Return,
    While,
    For,
    And,
    Or,
    In,

    // Types
    Identifier,
    Number,
    Float,
    Boolean,
    String,
    Char,

    // Datatypes
    Int,
    F32,
    Bool,
    Str,
    Nil,
    Void,

    // Single Characters
    Exclamation, // !
    At,          // @
    Hashtag,     // #
    Dollar,      // $
    Percent,     // %
    Caret,       // ^
    Ampersand,   // &
    Asterisk,    // *
    Lparen,      // (
    Rparen,      // )
    Minus,       // -
    Underscore,  // _
    Plus,        // +
    Assign,      // =
    Lbracket,    // [
    Rbracket,    // ]
    Lbrace,      // {
    Rbrace,      // }
    Semicolon,   // ;
    Colon,       // :
    Apostrophe,  // '
    Quote,       // "
    Comma,       // ,
    Period,      // .
    Lessthan,    // <
    Greaterthan, // >
    Slash,       // /
    Question,    // ?
    Backslash,   // /
    Pipe,        // |

    // Double Characters
    Equal,     // ==
    NotEqual,  // !=
    PlusEq,    // +=
    MinusEq,   // -=
    MultEq,    // *=
    DivEq,     // /=
    LtEqual,   // <=
    GtEqual,   // >=
    Increment, // ++
    Decrement, // --
    Comment,   // //
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
            m.insert("const", Const);
            m.insert("struct", Struct);
            m.insert("fn", Function);
            m.insert("if", If);
            m.insert("else", Else);
            m.insert("switch", Switch);
            m.insert("case", Case);
            m.insert("break", Break);
            m.insert("return", Return);
            m.insert("while", While);
            m.insert("for", For);
            m.insert("and", And);
            m.insert("or", Or);
            m.insert("in", In);

            m.insert("true", Boolean);
            m.insert("false", Boolean);

            m.insert("bool", Bool);
            m.insert("int", Int);
            m.insert("f32", F32);
            m.insert("str", Str);
            m.insert("nil", Nil);
            m.insert("void", Void);
            Mutex::new(m)
        })
        .lock()
        .expect("Let's hope the lock isn't poisoned")
}

pub fn get_double_token_map() -> MutexGuard<'static, HashMap<&'static str, TokenType>> {
    static DOUBLE_TOKEN_MAP: OnceLock<Mutex<HashMap<&str, TokenType>>> = OnceLock::new();
    DOUBLE_TOKEN_MAP
        .get_or_init(|| {
            use TokenType::*;
            let mut m = HashMap::new();
            m.insert("==", Equal);
            m.insert("!=", NotEqual);
            m.insert("+=", PlusEq);
            m.insert("-=", MinusEq);
            m.insert("*=", MultEq);
            m.insert("/=", DivEq);
            m.insert("<=", LtEqual);
            m.insert(">=", GtEqual);
            m.insert("++", Increment);
            m.insert("--", Decrement);
            m.insert("//", Comment);
            Mutex::new(m)
        })
        .lock()
        .expect("Let's hope the lock isn't poisoned")
}
