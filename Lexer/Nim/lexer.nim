import std/strutils
import token
import std/tables

type Lexer* = object
    input*: string
    char*: char
    curr*: int
    peek*: int

proc readCharacter*(self: var Lexer) =
    if self.peek >= self.input.len:
        self.char = '\0'
    else:
        self.char = self.input[self.peek]
    self.curr = self.peek
    self.peek += 1

proc newLexer*(input: string): Lexer =
    var lex: Lexer = Lexer(input: input, char: '\0', curr: 0, peek: 0)
    lex.readCharacter()
    return lex

proc readIdentifier(self: var Lexer): string =
    let pos = self.curr
    while isAlphaAscii(self.char) or self.char == '_':
        self.readCharacter()
    return self.input[pos..self.curr - 1]

proc readNumber(self: var Lexer): string =
    let pos = self.curr
    while isDigit(self.char):
        self.readCharacter()
    return self.input[pos..self.curr - 1]

proc nextToken*(self: var Lexer): Token =
    while isSpaceAscii(self.char):
        self.readCharacter()

    var tok: Token
    case self.char:
    of '\0': tok = newToken(TokenType.EOF, "\0")
    of '*': tok = newToken(TokenType.ASTERISK, "*")
    of '/': tok = newToken(TokenType.SLASH, "/")
    of '-': tok = newToken(TokenType.DASH, "-")
    of '+': tok = newToken(TokenType.PLUS, "+")
    of '=': tok = newToken(TokenType.EQUAL, "=")
    of '<': tok = newToken(TokenType.LESSTHAN, "<")
    of '>': tok = newToken(TokenType.GREATERTHAN, ">")
    of ';': tok = newToken(TokenType.SEMICOLON, ";")
    of ':': tok = newToken(TokenType.COLON, ":")
    of '(': tok = newToken(TokenType.LPAREN, "(")
    of ')': tok = newToken(TokenType.RPAREN, ")")
    of '{': tok = newToken(TokenType.LBRACE, "{")
    of '}': tok = newToken(TokenType.RBRACE, "}")
    else:
        if isAlphaAscii(self.char):
            let ident = self.readIdentifier()
            if IDENTIFIER_MAP.hasKey(ident):
                return newToken(IDENTIFIER_MAP[ident], ident)
            else:
                return newToken(TokenType.IDENTIFIER, ident)
        elif isDigit(self.char):
            return newToken(TokenType.Number, self.readNumber())
        return newToken(TokenType.Illegal, "ILLEGAL")

    self.readCharacter()
    return tok

