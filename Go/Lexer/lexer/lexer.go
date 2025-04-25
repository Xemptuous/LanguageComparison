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
	lex.advance()
	return &lex
}

func (l *Lexer) NextToken() token.Token {
	for unicode.IsSpace(rune(l.Char)) {
		l.advance()
	}
	var tok token.Token

	if l.Peek < len(l.Input) {
		if ds := string(l.Input[l.Curr]) + string(l.Input[l.Peek]); ds == "//" {
			return token.New(token.COMMENT, l.readComment())
		} else if tt, ok := token.DoubleTokenMap[ds]; ok {
			l.advance()
			l.advance()
			return token.New(tt, ds)
		}
	}

	switch l.Char {
	case 0:
		return token.New(token.EOF, "0")
	case '!':
		tok = token.New(token.EXCLAMATION, "!")
	case '@':
		tok = token.New(token.AT, "@")
	case '#':
		tok = token.New(token.HASHTAG, "#")
	case '$':
		tok = token.New(token.DOLLAR, "$")
	case '%':
		tok = token.New(token.PERCENT, "%")
	case '^':
		tok = token.New(token.CARET, "^")
	case '&':
		tok = token.New(token.AMPERSAND, "&")
	case '*':
		tok = token.New(token.ASTERISK, "*")
	case '(':
		tok = token.New(token.LPAREN, "(")
	case ')':
		tok = token.New(token.RPAREN, ")")
	case '-':
		tok = token.New(token.MINUS, "-")
	case '_':
		tok = token.New(token.UNDERSCORE, "_")
	case '+':
		tok = token.New(token.PLUS, "+")
	case '=':
		tok = token.New(token.ASSIGN, "=")
	case '[':
		tok = token.New(token.LBRACKET, "[")
	case ']':
		tok = token.New(token.RBRACKET, "]")
	case '{':
		tok = token.New(token.LBRACE, "{")
	case '}':
		tok = token.New(token.RBRACE, "}")
	case ';':
		tok = token.New(token.SEMICOLON, ";")
	case ':':
		tok = token.New(token.COLON, ":")
	case '\'':
		tok = token.New(token.CHAR, l.readChar())
	case '"':
		tok = token.New(token.STRING, l.readString())
	case ',':
		tok = token.New(token.COMMA, ",")
	case '.':
		tok = token.New(token.PERIOD, ".")
	case '<':
		tok = token.New(token.LESSTHAN, "<")
	case '>':
		tok = token.New(token.GREATERTHAN, ">")
	case '/':
		tok = token.New(token.SLASH, "/")
	case '?':
		tok = token.New(token.QUESTION, "?")
	case '\\':
		tok = token.New(token.BACKSLASH, "\\")
	case '|':
		tok = token.New(token.PIPE, "|")
	default:
		if unicode.IsLetter(rune(l.Char)) {
			ident := l.readIdentifier()
			if ttype, ok := token.IdentifierMap[ident]; ok {
				tok = token.New(ttype, ident)
			} else {
				tok = token.New(token.IDENTIFIER, ident)
			}
			return tok
		} else if unicode.IsNumber(rune(l.Char)) {
			ttype, lit := l.readNumber()
			return token.New(ttype, lit)
		} else {
			tok = token.New(token.ILLEGAL, "ILLEGAL")
		}
	}
	l.advance()

	return tok
}

func (l *Lexer) readIdentifier() string {
	pos := l.Curr
	for unicode.IsLetter(rune(l.Char)) || unicode.IsDigit(rune(l.Char)) || l.Char == '_' {
		l.advance()
	}
	return l.Input[pos:l.Curr]
}

func (l *Lexer) readNumber() (token.TokenType, string) {
	pos := l.Curr
	var is_float = false
	for unicode.IsDigit(rune(l.Char)) || l.Char == '.' {
		if l.Char == '.' {
			if is_float {
				l.advance()
				return token.ILLEGAL, "ILLEGAL"
			}
		}
		l.advance()
	}
	if is_float {
		return token.FLOAT, l.Input[pos:l.Curr]
	} else {
		return token.NUMBER, l.Input[pos:l.Curr]
	}
}

func (l *Lexer) readString() string {
	l.advance()
	pos := l.Curr
	for l.Char != '"' {
		l.advance()
	}
	return l.Input[pos:l.Curr]
}

func (l *Lexer) readChar() string {
	l.advance()
	pos := l.Curr
	for l.Char != '\'' {
		l.advance()
	}
	return l.Input[pos:l.Curr]
}

func (l *Lexer) readComment() string {
	pos := l.Curr
	for l.Char != '\n' && l.Char != '\r' {
		l.advance()
	}
	return l.Input[pos:l.Curr]
}

func (l *Lexer) advance() {
	if l.Peek >= len(l.Input) {
		l.Char = 0
	} else {
		l.Char = l.Input[l.Peek]
	}
	l.Curr = l.Peek
	l.Peek += 1
}
