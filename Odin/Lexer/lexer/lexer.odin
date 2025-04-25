package lexer

import "core:fmt"
import "core:io"
import "core:unicode"

Lexer :: struct {
	curr:  int,
	peek:  int,
	char:  u8,
	input: ^string,
}

newLexer :: proc(input: ^string) -> (l: Lexer) {
	l = Lexer {
		curr  = 0,
		peek  = 0,
		char  = '\000',
		input = input,
	}
	advance(&l)
	return
}

nextToken :: proc(lexer: ^Lexer) -> Token {
	if lexer.char == '\000' {
		return newToken(.EOF, "\000")
	}

	for unicode.is_white_space(rune(lexer.char)) {
		advance(lexer)
	}

	if lexer.peek < len(lexer.input) {
		ds := lexer.input[lexer.curr:lexer.peek + 1]

		if ds == "//" {
			return newToken(.COMMENT, readComment(lexer))
		}
		if tt, ok := DOUBLE_TOKEN_MAP[ds]; ok {
			advance(lexer)
			advance(lexer)
			return newToken(tt, ds)
		}
	}

	tok: Token
	switch lexer.char {
	case '\000':
		return newToken(.EOF, "\000")
	case '!':
		tok = newToken(.EXCLAMATION, "!")
	case '@':
		tok = newToken(.AT, "@")
	case '#':
		tok = newToken(.HASHTAG, "#")
	case '$':
		tok = newToken(.DOLLAR, "$")
	case '%':
		tok = newToken(.PERCENT, "%")
	case '^':
		tok = newToken(.CARET, "^")
	case '&':
		tok = newToken(.AMPERSAND, "&")
	case '*':
		tok = newToken(.ASTERISK, "*")
	case '(':
		tok = newToken(.LPAREN, "(")
	case ')':
		tok = newToken(.RPAREN, ")")
	case '-':
		tok = newToken(.MINUS, "-")
	case '_':
		tok = newToken(.UNDERSCORE, "_")
	case '+':
		tok = newToken(.PLUS, "+")
	case '=':
		tok = newToken(.ASSIGN, "=")
	case '[':
		tok = newToken(.LBRACKET, "[")
	case ']':
		tok = newToken(.RBRACKET, "]")
	case '{':
		tok = newToken(.LBRACE, "{")
	case '}':
		tok = newToken(.RBRACE, "}")
	case ';':
		tok = newToken(.SEMICOLON, ";")
	case ':':
		tok = newToken(.COLON, ":")
	case '\'':
		tok = newToken(.CHAR, readChar(lexer))
	case '"':
		tok = newToken(.STRING, readString(lexer))
	case ',':
		tok = newToken(.COMMA, ",")
	case '.':
		tok = newToken(.PERIOD, ".")
	case '<':
		tok = newToken(.LESSTHAN, "<")
	case '>':
		tok = newToken(.GREATERTHAN, ">")
	case '/':
		tok = newToken(.SLASH, "/")
	case '?':
		tok = newToken(.QUESTION, "?")
	case '\\':
		tok = newToken(.BACKSLASH, "\\")
	case '|':
		tok = newToken(.PIPE, "|")

	case '0' ..= '9':
		ttype, lit := readNumber(lexer)
		return newToken(ttype, lit)
	case 'A' ..= 'Z', 'a' ..= 'z':
		{
			str := readIdentifier(lexer)
			if type, ok := IDENTIFIER_MAP[str]; ok {
				return newToken(type, str)
			}
			return newToken(.IDENTIFIER, str)
		}
	case:
		return newToken(.ILLEGAL, "ILLEGAL")
	}
	advance(lexer)
	return tok
}

advance :: proc(lexer: ^Lexer) {
	if lexer.peek >= len(lexer.input) {
		lexer.char = '\000'
	} else {
		lexer.char = lexer.input[lexer.peek]
	}
	lexer.curr = lexer.peek
	lexer.peek += 1
}

readNumber :: proc(lexer: ^Lexer) -> (TokenType, string) {
	pos := lexer.curr
	is_float := false
	for unicode.is_digit(rune(lexer.char)) || lexer.char == '.' {
		if lexer.char == '.' {
			if is_float {
				advance(lexer)
				return .ILLEGAL, "ILLEGAL"
			}
			is_float = true
		}
		advance(lexer)
	}
	if is_float {
		return .FLOAT, lexer.input[pos:lexer.curr]
	}
	return .NUMBER, lexer.input[pos:lexer.curr]
}

readIdentifier :: proc(lexer: ^Lexer) -> string {
	pos := lexer.curr
	for unicode.is_alpha(rune(lexer.char)) ||
	    unicode.is_digit(rune(lexer.char)) ||
	    lexer.char == '_' {
		advance(lexer)
	}
	return lexer.input[pos:lexer.curr]
}

readString :: proc(lexer: ^Lexer) -> string {
	advance(lexer)
	pos := lexer.curr
	for lexer.char != '"' {
		advance(lexer)
	}
	return lexer.input[pos:lexer.curr]
}

readChar :: proc(lexer: ^Lexer) -> string {
	advance(lexer)
	pos := lexer.curr
	for lexer.char != '\'' {
		advance(lexer)
	}
	return lexer.input[pos:lexer.curr]
}

readComment :: proc(lexer: ^Lexer) -> string {
	pos := lexer.curr
	for lexer.char != '\n' && lexer.char != '\r' {
		advance(lexer)
	}
	return lexer.input[pos:lexer.curr]
}
