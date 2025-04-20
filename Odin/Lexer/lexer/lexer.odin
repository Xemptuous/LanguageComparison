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
	readChar(&l)
	return
}

nextToken :: proc(lexer: ^Lexer) -> Token {
	if lexer.char == '\000' {
		return newToken(.EOF, "\000")
	}

	for unicode.is_white_space(rune(lexer.char)) {
		readChar(lexer)
	}

	tok: Token
	switch lexer.char {
	case '\000':
		return newToken(.EOF, "\000")
	case '*':
		tok = newToken(.ASTERISK, "*")
	case '-':
		tok = newToken(.DASH, "-")
	case '+':
		tok = newToken(.PLUS, "+")
	case '=':
		tok = newToken(.EQUAL, "=")
	case '<':
		tok = newToken(.LESSTHAN, "<")
	case '>':
		tok = newToken(.GREATERTHAN, ">")
	case '/':
		tok = newToken(.SLASH, "/")
	case ';':
		tok = newToken(.SEMICOLON, ";")
	case ':':
		tok = newToken(.COLON, ":")
	case '(':
		tok = newToken(.LPAREN, "(")
	case ')':
		tok = newToken(.RPAREN, ")")
	case '{':
		tok = newToken(.LBRACE, "{")
	case '}':
		tok = newToken(.RBRACE, "}")
	case '0' ..= '9':
		return newToken(.NUMBER, readNumber(lexer))
	case 'A' ..= 'Z', 'a' ..= 'z':
		{
			str := readIdentifier(lexer)
			if type, ok := LITERAL_MAP[str]; ok {
				return newToken(type, str)
			} else {
				return newToken(.IDENTIFIER, str)
			}
		}
	case:
		return newToken(.ILLEGAL, "ILLEGAL")
	}
	readChar(lexer)
	return tok
}

readNumber :: proc(lexer: ^Lexer) -> string {
	pos := lexer.curr
	for unicode.is_digit(rune(lexer.char)) {
		readChar(lexer)
	}
	return lexer.input[pos:lexer.curr]
}

readIdentifier :: proc(lexer: ^Lexer) -> string {
	pos := lexer.curr
	for unicode.is_letter(rune(lexer.char)) || lexer.char == '_' {
		readChar(lexer)
	}
	return lexer.input[pos:lexer.curr]
}

readChar :: proc(lexer: ^Lexer) {
	if lexer.peek >= len(lexer.input) {
		lexer.char = '\000'
	} else {
		lexer.char = lexer.input[lexer.peek]
	}
	lexer.curr = lexer.peek
	lexer.peek += 1
}
