import { TokenType } from "./token";
import { Lexer } from "./lexer";

function main(): void {
  const input = `
	fn is_gt_ten(num: int) bool {
		if (num > 10) {
			return true;
		} else {
			return false;
		}
	}
	let ten = 5 + 5 * 4 / 2 - 5;
	print(is_big(ten));
  `;

  let lexer = new Lexer(input);

  let tok = lexer.nextToken();
  while (tok.type != TokenType.EOF) {
    console.log(tok);
    tok = lexer.nextToken();
  }
}

main();
