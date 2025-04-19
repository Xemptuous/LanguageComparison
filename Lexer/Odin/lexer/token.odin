#+feature dynamic-literals
package lexer

Token :: struct {
	literal: string,
	type:    TokenType,
}


TokenType :: enum {
	EOF,
	ILLEGAL,
	LET,
	FUNCTION,
	IF,
	ELSE,
	RETURN,
	IDENTIFIER,
	NUMBER,
	BOOLEAN,
	INT,
	BOOL,
	ASTERISK, // *
	SLASH, // /
	DASH, // -
	PLUS, // +
	EQUAL, // =
	LESSTHAN, // <
	GREATERTHAN, // >
	SEMICOLON, // ;
	COLON, // :
	LPAREN, // (
	RPAREN, // )
	LBRACE, // {
	RBRACE, // }
}

newToken :: proc(type: TokenType, literal: string) -> (n: Token) {
	return Token{literal, type}
}

LITERAL_MAP := map[string]TokenType {
	"let"    = .LET,
	"fn"     = .FUNCTION,
	"if"     = .IF,
	"else"   = .ELSE,
	"return" = .RETURN,
	"true"   = .BOOLEAN,
	"false"  = .BOOLEAN,
	"bool"   = .BOOL,
	"int"    = .INT,
}
