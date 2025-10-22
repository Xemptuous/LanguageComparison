#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Association {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Ident(String),
    Operator(Op),
    LParen,
    RParen,
    Comma,
    Call { name: String, argc: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

impl Op {
    pub fn prec(&self) -> u8 {
        use Op::*;
        match self {
            Pow => 3,
            Mul | Div | Mod => 2,
            Add | Sub => 1,
        }
    }
    pub fn association(&self) -> Association {
        match self {
            Op::Pow => Association::Right,
            _ => Association::Left,
        }
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut i = 0;
    let bytes = input.as_bytes();

    while i < input.len() {
        match bytes[i] as char {
            c if c.is_whitespace() => i += 1,
            c if c.is_ascii_digit() || c == '.' => {
                let start = i;
                i += 1;
                while i < bytes.len() && ((bytes[i] as char).is_ascii_digit() || bytes[i] == b'.') {
                    i += 1;
                }
                let s = &input[start..i];
                let n: f64 = s.parse().map_err(|_| format!("bad number: {}", s))?;
                tokens.push(Token::Number(n));
            },

            c if c.is_ascii_alphabetic() || c == '_' => {
                let start = i;
                i += 1;
                while i < bytes.len() {
                    let ch = bytes[i] as char;
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        i += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Ident(input[start..i].to_string()));
            },
            '+' => push_and_increment(Token::Operator(Op::Add), &mut tokens, &mut i),
            '-' => push_and_increment(Token::Operator(Op::Sub), &mut tokens, &mut i),
            '*' => push_and_increment(Token::Operator(Op::Mul), &mut tokens, &mut i),
            '/' => push_and_increment(Token::Operator(Op::Div), &mut tokens, &mut i),
            '%' => push_and_increment(Token::Operator(Op::Mod), &mut tokens, &mut i),
            '^' => push_and_increment(Token::Operator(Op::Pow), &mut tokens, &mut i),
            '(' => push_and_increment(Token::LParen, &mut tokens, &mut i),
            ')' => push_and_increment(Token::RParen, &mut tokens, &mut i),
            ',' => push_and_increment(Token::Comma, &mut tokens, &mut i),
            ch => return Err(format!("unrecognized char: {}", ch)),
        }
    }
    Ok(tokens)
}

fn push_and_increment(tok: Token, toks: &mut Vec<Token>, i: &mut usize) {
    toks.push(tok);
    *i += 1;
}
