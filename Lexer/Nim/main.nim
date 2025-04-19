import token
import lexer


proc main() = 
    let input = """
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
    var lex: Lexer = newLexer(input)

    var tok = lex.nextToken()
    while tok.token_type != TokenType.Eof:
        echo $"Token{ literal: \"" & $tok.literal & $"\" type: " & $tok.token_type & $" }"
        tok = lex.nextToken()

main()
