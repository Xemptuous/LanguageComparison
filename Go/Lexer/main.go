package main

import (
	"fmt"
	"golex/lexer"
	token "golex/token"
)

func main() {
	input := `
	fn is_gt_ten(num: int) bool {
		if (num > 10) {
			return true;
		} else {
			return false;
		}
	}
	let ten = 5 + 5 * 4 / 2 - 5;
	print(is_big(ten));
	`
	lex := lexer.New(input)

	var tok token.Token = lex.NextToken()
	for tok.Type != token.EOF {
		// fmt.Printf("%#v\n", tok)
		fmt.Println(tok)
		tok = lex.NextToken()
	}
}
