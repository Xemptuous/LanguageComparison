#+feature dynamic-literals
package lexer

Token :: struct {
	literal: string,
	type:    TokenType,
}


TokenType :: enum {
	EOF,
	ILLEGAL,

	// Core
	LET,
	CONST,
	STRUCT,
	FUNCTION,
	IF,
	ELSE,
	SWITCH,
	CASE,
	BREAK,
	RETURN,
	WHILE,
	FOR,
	AND,
	OR,
	IN,

	// Types
	IDENTIFIER,
	NUMBER,
	FLOAT,
	BOOLEAN,
	STRING,
	CHAR,

	// Datatypes
	INT,
	F32,
	BOOL,
	STR,
	NIL,
	VOID,

	// Single Characters
	EXCLAMATION, // !
	AT, // @
	HASHTAG, // #
	DOLLAR, // $
	PERCENT, // %
	CARET, // ^
	AMPERSAND, // &
	ASTERISK, // *
	LPAREN, // (
	RPAREN, // )
	MINUS, // -
	UNDERSCORE, // _
	PLUS, // +
	ASSIGN, // =
	LBRACKET, // [
	RBRACKET, // ]
	LBRACE, // {
	RBRACE, // }
	SEMICOLON, // ;
	COLON, // :
	APOSTROPHE, // '
	QUOTE, // "
	COMMA, // ,
	PERIOD, // .
	LESSTHAN, // <
	GREATERTHAN, // >
	SLASH, // /
	QUESTION, // ?
	BACKSLASH, // \/
	PIPE, // |

	// Double Characters
	EQUAL, // ==
	NOT_EQUAL, // !=
	PLUS_EQ, // +=
	MINUS_EQ, // -=
	MULT_EQ, // *=
	DIV_EQ, // /=
	LT_EQUAL, // <=
	GT_EQUAL, // >=
	INCREMENT, // ++
	DECREMENT, // --
	COMMENT, // //
}

newToken :: proc(type: TokenType, literal: string) -> (n: Token) {
	return Token{literal, type}
}

IDENTIFIER_MAP := map[string]TokenType {
	"let"    = .LET,
	"const"  = .CONST,
	"struct" = .STRUCT,
	"fn"     = .FUNCTION,
	"if"     = .IF,
	"else"   = .ELSE,
	"switch" = .SWITCH,
	"case"   = .CASE,
	"break"  = .BREAK,
	"return" = .RETURN,
	"while"  = .WHILE,
	"for"    = .FOR,
	"and"    = .AND,
	"or"     = .OR,
	"in"     = .IN,
	"true"   = .BOOLEAN,
	"false"  = .BOOLEAN,
	"bool"   = .BOOL,
	"int"    = .INT,
	"f32"    = .F32,
	"str"    = .STR,
	"nil"    = .NIL,
	"void"   = .VOID,
}

DOUBLE_TOKEN_MAP := map[string]TokenType {
	"==" = .EQUAL,
	"!=" = .NOT_EQUAL,
	"+=" = .PLUS_EQ,
	"-=" = .MINUS_EQ,
	"*=" = .MULT_EQ,
	"/=" = .DIV_EQ,
	"<=" = .LT_EQUAL,
	">=" = .GT_EQUAL,
	"++" = .INCREMENT,
	"--" = .DECREMENT,
	"//" = .COMMENT,
}
