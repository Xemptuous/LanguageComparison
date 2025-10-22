use crate::lexer::*;
use std::collections::HashMap;

type BuiltinFnMap = HashMap<&'static str, fn(&[f64]) -> Result<f64, String>>;

fn builtin_funcs() -> BuiltinFnMap {
    let mut m: BuiltinFnMap = HashMap::new();
    m.insert("sin", |args| {
        if args.len() == 1 {
            Ok(args[0].sin())
        } else {
            Err("sin(x)".into())
        }
    });
    m.insert("cos", |args| {
        if args.len() == 1 {
            Ok(args[0].cos())
        } else {
            Err("cos(x)".into())
        }
    });
    m.insert("sqrt", |args| {
        if args.len() == 1 {
            Ok(args[0].sqrt())
        } else {
            Err("sqrt(x)".into())
        }
    });
    m.insert("max", |args| {
        if !args.is_empty() {
            Ok(args.iter().copied().fold(f64::NEG_INFINITY, f64::max))
        } else {
            Err("max requires >=1 arg".into())
        }
    });
    m.insert("min", |args| {
        if !args.is_empty() {
            Ok(args.iter().copied().fold(f64::INFINITY, f64::min))
        } else {
            Err("min requires >=1 arg".into())
        }
    });
    m.insert("log", |args| match args {
        [x] => Ok(x.ln()),
        [x, b] => Ok(x.log(*b)),
        _ => Err("log(x) or log(x,b)".into()),
    });
    m
}

pub fn eval_rpn(rpn: &[Token]) -> Result<f64, String> {
    let funcs = builtin_funcs();
    let mut st: Vec<f64> = Vec::new();
    for t in rpn {
        match t {
            Token::Number(n) => st.push(*n),
            Token::Operator(op) => {
                let r = st.pop().ok_or("stack underflow")?;
                let l = st.pop().ok_or("stack underflow")?;
                st.push(match op {
                    Op::Add => l + r,
                    Op::Sub => l - r,
                    Op::Mul => l * r,
                    Op::Div => l / r,
                    Op::Pow => l.powf(r),
                    Op::Mod => l % r,
                });
            },
            Token::Call { name, argc } => {
                let argc = *argc;
                if st.len() < argc {
                    return Err("stack underflow (call)".into());
                }
                let start = st.len() - argc;
                let args: Vec<f64> = st.drain(start..).collect();
                let f = funcs
                    .get(name.as_str())
                    .ok_or_else(|| format!("unknown function {}", name))?;
                let val = f(&args)?;
                st.push(val);
            },
            Token::Ident(_) | Token::LParen | Token::RParen | Token::Comma => {
                return Err("unexpected token in RPN".into());
            },
        }
    }
    st.pop().ok_or("empty expression".into())
}
