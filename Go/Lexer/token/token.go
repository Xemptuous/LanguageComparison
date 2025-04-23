package token

type TokenType int

const (
	EOF = iota
	ILLEGAL

	// Core
	LET
	CONST
	STRUCT
	FUNCTION
	IF
	ELSE
	SWITCH
	CASE
	BREAK
	RETURN
	WHILE
	FOR
	AND
	OR
	IN

	// Types
	IDENTIFIER
	NUMBER
	FLOAT
	BOOLEAN
	STRING
	CHAR

	// Datatypes
	INT
	F32
	BOOL
	STR
	NIL
	VOID

	// Single Characters
	EXCLAMATION // !
	AT          // @
	HASHTAG     // #
	DOLLAR      // $
	PERCENT     // %
	CARET       // ^
	AMPERSAND   // &
	ASTERISK    // *
	LPAREN      // (
	RPAREN      // )
	MINUS       // -
	UNDERSCORE  // _
	PLUS        // +
	ASSIGN      // =
	LBRACKET    // [
	RBRACKET    // ]
	LBRACE      // {
	RBRACE      // }
	SEMICOLON   // ;
	COLON       // :
	APOSTROPHE  // '
	QUOTE       // "
	COMMA       // ,
	PERIOD      // .
	LESSTHAN    // <
	GREATERTHAN // >
	SLASH       // /
	QUESTION    // ?
	BACKSLASH   // \/
	PIPE        // |

	// Double Characters
	EQUAL     // ==
	NOT_EQUAL // !=
	PLUS_EQ   // +=
	MINUS_EQ  // -=
	MULT_EQ   // *=
	DIV_EQ    // /=
	LT_EQUAL  // <=
	GT_EQUAL  // >=
	INCREMENT // ++
	DECREMENT // --
	COMMENT   // //
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
	"const":  CONST,
	"struct": STRUCT,
	"fn":     FUNCTION,
	"if":     IF,
	"else":   ELSE,
	"switch": SWITCH,
	"case":   CASE,
	"break":  BREAK,
	"return": RETURN,
	"while":  WHILE,
	"for":    FOR,
	"and":    AND,
	"or":     OR,
	"in":     IN,

	"true":  BOOLEAN,
	"false": BOOLEAN,

	"bool": BOOL,
	"int":  INT,
	"f32":  F32,
	"str":  STR,
	"nil":  NIL,
	"void": VOID,
}

var DoubleTokenMap = map[string]TokenType{
	"==": EQUAL,
	"!=": NOT_EQUAL,
	"+=": PLUS_EQ,
	"-=": MINUS_EQ,
	"*=": MULT_EQ,
	"/=": DIV_EQ,
	"<=": LT_EQUAL,
	">=": GT_EQUAL,
	"++": INCREMENT,
	"--": DECREMENT,
	"//": COMMENT,
}

func (t TokenType) String() string {
	switch t {
	case EOF:
		return "EOF"
	case ILLEGAL:
		return "ILLEGAL"

	// Core
	case LET:
		return "LET"
	case CONST:
		return "CONST"
	case STRUCT:
		return "STRUCT"
	case FUNCTION:
		return "FUNCTION"
	case IF:
		return "IF"
	case ELSE:
		return "ELSE"
	case SWITCH:
		return "SWITCH"
	case CASE:
		return "CASE"
	case BREAK:
		return "BREAK"
	case RETURN:
		return "RETURN"
	case WHILE:
		return "WHILE"
	case FOR:
		return "FOR"
	case AND:
		return "AND"
	case OR:
		return "OR"
	case IN:
		return "IN"

	// Types
	case IDENTIFIER:
		return "IDENTIFIER"
	case NUMBER:
		return "NUMBER"
	case FLOAT:
		return "FLOAT"
	case BOOLEAN:
		return "BOOLEAN"
	case STRING:
		return "STRING"
	case CHAR:
		return "CHAR"

	// Datatypes
	case INT:
		return "INT"
	case F32:
		return "F32"
	case BOOL:
		return "BOOL"
	case STR:
		return "STR"
	case NIL:
		return "NIL"
	case VOID:
		return "VOID"

	// Single Characters
	case EXCLAMATION:
		return "EXCLAMATION"
	case AT:
		return "AT"
	case HASHTAG:
		return "HASHTAG"
	case DOLLAR:
		return "DOLLAR"
	case PERCENT:
		return "PERCENT"
	case CARET:
		return "CARET"
	case AMPERSAND:
		return "AMPERSAND"
	case ASTERISK:
		return "ASTERISK"
	case LPAREN:
		return "LPAREN"
	case RPAREN:
		return "RPAREN"
	case MINUS:
		return "MINUS"
	case UNDERSCORE:
		return "UNDERSCORE"
	case PLUS:
		return "PLUS"
	case ASSIGN:
		return "ASSIGN"
	case LBRACKET:
		return "LBRACKET"
	case RBRACKET:
		return "RBRACKET"
	case LBRACE:
		return "LBRACE"
	case RBRACE:
		return "RBRACE"
	case SEMICOLON:
		return "SEMICOLON"
	case COLON:
		return "COLON"
	case APOSTROPHE:
		return "APOSTROPHE"
	case QUOTE:
		return "QUOTE"
	case COMMA:
		return "COMMA"
	case PERIOD:
		return "PERIOD"
	case LESSTHAN:
		return "LESSTHAN"
	case GREATERTHAN:
		return "GREATERTHAN"
	case SLASH:
		return "SLASH"
	case QUESTION:
		return "QUESTION"
	case BACKSLASH:
		return "BACKSLASH"
	case PIPE:
		return "PIPE"

	// Double Characters
	case EQUAL:
		return "EQUAL"
	case NOT_EQUAL:
		return "NOT_EQUAL"
	case PLUS_EQ:
		return "PLUS_EQ"
	case MINUS_EQ:
		return "MINUS_EQ"
	case MULT_EQ:
		return "MULT_EQ"
	case DIV_EQ:
		return "DIV_EQ"
	case LT_EQUAL:
		return "LT_EQUAL"
	case GT_EQUAL:
		return "GT_EQUAL"
	case INCREMENT:
		return "INCREMENT"
	case DECREMENT:
		return "DECREMENT"
	case COMMENT:
		return "COMMENT"
	default:
		return ""
	}
}
