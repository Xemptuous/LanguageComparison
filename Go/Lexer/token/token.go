package token

type TokenType int

const (
	EOF = iota
	ILLEGAL

	LET
	FUNCTION
	IF
	ELSE
	RETURN

	IDENTIFIER
	NUMBER
	BOOLEAN

	INT
	BOOL

	ASTERISK    // *
	SLASH       // /
	DASH        // -
	PLUS        // +
	EQUAL       // =
	LESSTHAN    // <
	GREATERTHAN // >
	SEMICOLON   // ;
	COLON       // :
	LPAREN      // (
	RPAREN      // )
	LBRACE      // {
	RBRACE      // }
)

type Token struct {
	Literal string
	Type    TokenType
}

func New(ttype TokenType, lit string) Token {
	return Token{
		Literal: lit,
		Type:    ttype,
	}
}

var IdentifierMap = map[string]TokenType{
	"let":    LET,
	"fn":     FUNCTION,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,

	"true":  BOOLEAN,
	"false": BOOLEAN,

	"bool": BOOL,
	"int":  INT,
}
