use crate::lexer::{Association, Token};

pub fn to_rpn(tokens: &[Token]) -> Result<Vec<Token>, String> {
    let mut out: Vec<Token> = Vec::new();
    let mut ops: Vec<Token> = Vec::new();
    let mut arities: Vec<usize> = Vec::new();

    let mut i = 0;
    use Token::*;
    while i < tokens.len() {
        match &tokens[i] {
            Number(n) => out.push(Number(*n)),
            Ident(name) => {
                if matches!(tokens.get(i + 1), Some(Token::LParen)) {
                    ops.push(Token::Ident(name.clone()));
                    arities.push(0);
                } else {
                    return Err(format!(
                        "Unknown identifier '{}' (not a function call)",
                        name
                    ));
                }
            },
            Operator(op1) => {
                while let Some(top) = ops.last() {
                    match top {
                        Operator(op2) => {
                            let cond = match op1.association() {
                                Association::Left => op2.prec() >= op1.prec(),
                                Association::Right => op2.prec() > op1.prec(),
                            };
                            if cond {
                                out.push(ops.pop().unwrap());
                            } else {
                                break;
                            }
                        },
                        _ => break,
                    }
                }
                ops.push(Token::Operator(*op1));
            },
            LParen => {
                ops.push(Token::LParen);
            },
            Comma => {
                while let Some(top) = ops.last() {
                    if matches!(top, Token::LParen) {
                        break;
                    }
                    out.push(ops.pop().unwrap());
                }
                if let Some(a) = arities.last_mut() {
                    *a += 1;
                } else {
                    return Err("Comma not inside a function call".into());
                }
            },
            RParen => {
                while let Some(top) = ops.last() {
                    if matches!(top, Token::LParen) {
                        break;
                    }
                    out.push(ops.pop().unwrap());
                }
                if !matches!(ops.pop(), Some(Token::LParen)) {
                    return Err("Mismatched parentheses".into());
                }
                if let Some(Token::Ident(name)) = ops.last().cloned() {
                    ops.pop();
                    let argc = if let Some(a) = arities.pop() {
                        a + 1
                    } else {
                        0
                    };
                    out.push(Token::Call { name, argc });
                }
            },
            Call { .. } => unreachable!(),
        }
        if !arities.is_empty() {
            match tokens[i] {
                Number(_) | Ident(_) | RParen => {},
                _ => {},
            }
        }
        i += 1;
    }

    while let Some(t) = ops.pop() {
        match t {
            LParen | RParen => return Err("Mismatched parentheses".into()),
            Ident(n) => {
                out.push(Call { name: n, argc: 0 });
            },
            _ => out.push(t),
        }
    }
    Ok(out)
}
