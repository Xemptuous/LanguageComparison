use crate::{evaluator::eval_rpn, lexer::lex, parser::to_rpn};

mod evaluator;
mod lexer;
mod parser;

fn main() {
    let input = "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3";
    let tokens = lex(input).expect("Lexing error");
    let rpn = to_rpn(&tokens).expect("RPN parse error");
    let result = eval_rpn(&rpn).expect("Calculate error");
    println!("{}", result);
}
