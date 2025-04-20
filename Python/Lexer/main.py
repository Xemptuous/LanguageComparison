from lexer import Lexer
from tokens import TokenType


def main():
    input = """
        fn is_gt_ten(num: int) bool {
            if (num > 10) {
                return true;
            } else {
                return false;
            }
        }
        let ten = 5 + 5 * 4 / 2 - 5;
        print(is_big(ten));
    """
    lexer = Lexer(input)

    tok = lexer.next_token()
    print(tok.token_type, tok.literal)
    while tok.token_type != TokenType.EOF:
        print(tok)
        tok = lexer.next_token()


if __name__ == "__main__":
    main()
