import std.stdio;

import lexer;
import token;

void main()
{
    string input = "
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

    Lexer lex = new Lexer(input);

    Token tok = lex.nextToken();
    while (tok.type != TokenType.Eof)
    {
        writeln(tok);
        tok = lex.nextToken();
    }
}
