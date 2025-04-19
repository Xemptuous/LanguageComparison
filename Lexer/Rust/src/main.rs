mod lexer;
mod token;

use lexer::Lexer;

fn main() {
    let input = "
        fn is_gt_ten(num: int) bool {
            if (num > 10) {
                return true;
            } else {
                return false;
            }
        }
        let ten = 5 + 5 * 4 / 2 - 5;
        print(is_big(ten));
    ";
    let lex = Lexer::new(input);

    let mut tok = lex.next_token();
    while tok.token_type != token::TokenType::Eof {
        println!("{:?}", tok);
        tok = lex.next_token();
    }
}
