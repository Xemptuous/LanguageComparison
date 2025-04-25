import std/strutils
import token
import std/tables

type Lexer* = object
    input*: string
    char*: char
    curr*: int
    peek*: int

proc advance*(self: var Lexer) =
    if self.peek >= self.input.len:
        self.char = '\0'
    else:
        self.char = self.input[self.peek]
    self.curr = self.peek
    self.peek += 1

proc newLexer*(input: string): Lexer =
    var lex: Lexer = Lexer(input: input, char: '\0', curr: 0, peek: 0)
    lex.advance()
    return lex

proc readIdentifier(self: var Lexer): string =
    let pos = self.curr
    while isAlphaAscii(self.char) or isDigit(self.char) or self.char == '_':
        self.advance()
    return self.input[pos..self.curr - 1]

proc readNumber(self: var Lexer): (TokenType, string) =
    let pos = self.curr
    var is_float = false
    while isDigit(self.char) or self.char == '.':
        if self.char == '.':
            if is_float:
                return (TokenType.Illegal, "ILLEGAL")
            is_float = true
        self.advance()
    if is_float:
        return (TokenType.Float, self.input[pos..self.curr - 1])
    return (TokenType.Number, self.input[pos..self.curr - 1])

proc readString(self: var Lexer): string =
    self.advance()
    let pos = self.curr
    while self.char != '"' and self.char != '\0':
        self.advance()
    return self.input[pos..self.curr - 1]

proc readChar(self: var Lexer): string =
    self.advance()
    let pos = self.curr
    while self.char != '\'' and self.char != '\0':
        self.advance()
    return self.input[pos..self.curr - 1]

proc readComment(self: var Lexer): string =
    let pos = self.curr
    while self.char != '\n' and self.char != '\r':
        self.advance()
    return self.input[pos..self.curr - 1]

proc nextToken*(self: var Lexer): Token =
    while isSpaceAscii(self.char):
        self.advance()

    var tok: Token

    if self.peek < self.input.len:
        let ds = self.input[self.curr..self.peek]
        if ds == "//":
            return newToken(TokenType.Comment, self.readComment())
        if DOUBLE_TOKEN_MAP.hasKey(ds):
            self.advance()
            self.advance()
            return newToken(DOUBLE_TOKEN_MAP[ds], ds)

    case self.char:
    of '\0': tok = newToken(TokenType.Eof, "\0")
    of '!': tok = newToken(TokenType.Exclamation, "!")
    of '@': tok = newToken(TokenType.At, "@")
    of '#': tok = newToken(TokenType.Hashtag, "#")
    of '$': tok = newToken(TokenType.Dollar, "$")
    of '%': tok = newToken(TokenType.Percent, "%")
    of '^': tok = newToken(TokenType.Caret, "^")
    of '&': tok = newToken(TokenType.Ampersand, "&")
    of '*': tok = newToken(TokenType.Asterisk, "*")
    of '(': tok = newToken(TokenType.Lparen, "(")
    of ')': tok = newToken(TokenType.Rparen, ")")
    of '-': tok = newToken(TokenType.Minus, "-")
    of '_': tok = newToken(TokenType.Underscore, "_")
    of '+': tok = newToken(TokenType.Plus, "+")
    of '=': tok = newToken(TokenType.Assign, "=")
    of '[': tok = newToken(TokenType.Lbracket, "[")
    of ']': tok = newToken(TokenType.Rbracket, "]")
    of '{': tok = newToken(TokenType.Lbrace, "{")
    of '}': tok = newToken(TokenType.Rbrace, "}")
    of ';': tok = newToken(TokenType.Semicolon, ";")
    of ':': tok = newToken(TokenType.Colon, ":")
    of '\'': tok = newToken(TokenType.Char, self.readChar())
    of '"': tok = newToken(TokenType.String, self.readString())
    of ',': tok = newToken(TokenType.Comma, ",")
    of '.': tok = newToken(TokenType.Period, ".")
    of '<': tok = newToken(TokenType.Lessthan, "<")
    of '>': tok = newToken(TokenType.Greaterthan, ">")
    of '/': tok = newToken(TokenType.Slash, "/")
    of '?': tok = newToken(TokenType.Question, "?")
    of '\\': tok = newToken(TokenType.Backslash, "\\")
    of '|': tok = newToken(TokenType.Pipe, "|")
    else:
        if isAlphaAscii(self.char):
            let ident = self.readIdentifier()
            if IDENTIFIER_MAP.hasKey(ident):
                return newToken(IDENTIFIER_MAP[ident], ident)
            else:
                return newToken(TokenType.Identifier, ident)
        elif isDigit(self.char):
            let (ttype, num) = self.readNumber()
            return newToken(ttype, num)
        return newToken(TokenType.Illegal, "ILLEGAL")

    self.advance()
    return tok

