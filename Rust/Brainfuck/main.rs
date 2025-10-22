#!/bin/rust-script

use std::{
    collections::HashMap,
    io::{Read, Write},
};

fn make_jump_table(input: &str) -> Result<HashMap<usize, usize>, ()> {
    let mut jump = HashMap::new();
    let mut stack = Vec::new();
    for (i, ch) in input.bytes().enumerate() {
        match ch {
            b'[' => stack.push(i),
            b']' => {
                let idx = match stack.pop() {
                    Some(i) => i,
                    None => return Err(()),
                };
                jump.insert(idx, i);
                jump.insert(i, idx);
            },
            _ => {},
        }
    }
    if !stack.is_empty() {
        return Err(());
    }
    Ok(jump)
}
pub fn main() {
    let input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    let mut tape = [0u8; 3000];
    let jump = make_jump_table(input).expect("Mismatched brackets");

    let mut dp: usize = 0;
    let mut ip: usize = 0;

    let bytes = input.as_bytes();
    while ip < bytes.len() {
        match bytes[ip] {
            b'>' => dp = dp.checked_add(1).expect("dptr overflow"),
            b'<' => dp = dp.checked_sub(1).expect("dptr underflow"),
            b'+' => tape[dp] = tape[dp].wrapping_add(1),
            b'-' => tape[dp] = tape[dp].wrapping_sub(1),
            b'.' => {
                print!("{}", tape[dp] as char);
                std::io::stdout().flush().expect("Couldn't write output");
            },
            b',' => {
                let mut buf = [0u8; 1];
                std::io::stdin()
                    .read_exact(&mut buf)
                    .expect("Couldn't read input");
                tape[dp] = buf[0];
            },
            b'[' => {
                if tape[dp] == 0 {
                    ip = jump[&ip];
                }
            },
            b']' => {
                if tape[dp] != 0 {
                    ip = jump[&ip];
                }
            },
            _ => {},
        }
        ip += 1
    }
}
