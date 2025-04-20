package lexer

import (
	token "golex/token"
	"unicode"
)

type Lexer struct {
	Curr  int
	Peek  int
	Char  byte
	Input string
}

func New(input string) *Lexer {
	lex := Lexer{Input: input}
	lex.readChar()
	return &lex
}

func (l *Lexer) NextToken() token.Token {
	for unicode.IsSpace(rune(l.Char)) {
		l.readChar()
	}
	var tok token.Token

	switch l.Char {
	case '*':
		tok = token.New(token.ASTERISK, "*")
	case '-':
		tok = token.New(token.DASH, "-")
	case '+':
		tok = token.New(token.PLUS, "+")
	case '=':
		tok = token.New(token.EQUAL, "=")
	case '<':
		tok = token.New(token.LESSTHAN, "<")
	case '>':
		tok = token.New(token.GREATERTHAN, ">")
	case '/':
		tok = token.New(token.SLASH, "/")
	case ';':
		tok = token.New(token.SEMICOLON, ";")
	case ':':
		tok = token.New(token.COLON, ":")
	case '(':
		tok = token.New(token.LPAREN, "(")
	case ')':
		tok = token.New(token.RPAREN, ")")
	case '{':
		tok = token.New(token.LBRACE, "{")
	case '}':
		tok = token.New(token.RBRACE, "}")
	case 0:
		tok = token.New(token.EOF, "")
	default:
		if isLetter(l.Char) {
			ident := l.readIdentifier()
			if ttype, ok := token.IdentifierMap[ident]; ok {
				tok = token.New(ttype, ident)
			} else {
				tok = token.New(token.IDENTIFIER, ident)
			}
			return tok
		} else if isNumber(l.Char) {
			return token.New(token.NUMBER, l.readNumber())
		} else {
			tok = token.New(token.ILLEGAL, "ILLEGAL")
		}
	}
	l.readChar()

	return tok
}

func (l *Lexer) readIdentifier() string {
	pos := l.Curr
	for isLetter(l.Char) || l.Char == '_' {
		l.readChar()
	}
	return l.Input[pos:l.Curr]
}

func (l *Lexer) readNumber() string {
	pos := l.Curr
	for isNumber(l.Char) {
		l.readChar()
	}
	return l.Input[pos:l.Curr]
}

func (l *Lexer) readChar() {
	if l.Peek >= len(l.Input) {
		l.Char = 0
	} else {
		l.Char = l.Input[l.Peek]
	}
	l.Curr = l.Peek
	l.Peek += 1
}

func isLetter(ch byte) bool {
	// 'a' <= c <= 'z' OR 'A' <= c <= 'Z'
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z'
}

func isNumber(ch byte) bool {
	return ch >= '0' && ch <= '9'
}
