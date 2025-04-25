from tokens import DOUBLE_TOKEN_MAP, IDENTIFIER_MAP, Token, TokenType


class Lexer:
    curr: int
    peek: int
    input: str
    ch: str | None

    def __init__(self, input: str) -> None:
        self.input = input
        self.curr = 0
        self.peek = 0
        self.ch = None
        self.advance()

    def next_token(self) -> Token:
        tok: Token = Token(TokenType.EOF, "")

        while self.ch is not None and self.ch.isspace():
            self.advance()

        if not self.ch:
            return tok

        if self.peek < len(self.input):
            if (ds := self.input[self.curr : self.peek + 1]) == "//":
                return Token(TokenType.COMMENT, self.read_comment())
            elif (ttype := DOUBLE_TOKEN_MAP.get(ds, None)) is not None:
                self.advance()
                self.advance()
                return Token(ttype, ds)

        match self.ch:
            case "!":
                tok = Token(TokenType.EXCLAMATION, "!")
            case "@":
                tok = Token(TokenType.AT, "@")
            case "#":
                tok = Token(TokenType.HASHTAG, "#")
            case "$":
                tok = Token(TokenType.DOLLAR, "$")
            case "%":
                tok = Token(TokenType.PERCENT, "%")
            case "^":
                tok = Token(TokenType.CARET, "^")
            case "&":
                tok = Token(TokenType.AMPERSAND, "&")
            case "*":
                tok = Token(TokenType.ASTERISK, "*")
            case "(":
                tok = Token(TokenType.LPAREN, "(")
            case ")":
                tok = Token(TokenType.RPAREN, ")")
            case "-":
                tok = Token(TokenType.MINUS, "-")
            case "_":
                tok = Token(TokenType.UNDERSCORE, "_")
            case "+":
                tok = Token(TokenType.PLUS, "+")
            case "=":
                tok = Token(TokenType.ASSIGN, "=")
            case "[":
                tok = Token(TokenType.LBRACKET, "[")
            case "]":
                tok = Token(TokenType.RBRACKET, "]")
            case "{":
                tok = Token(TokenType.LBRACE, "{")
            case "}":
                tok = Token(TokenType.RBRACE, "}")
            case ";":
                tok = Token(TokenType.SEMICOLON, ";")
            case ":":
                tok = Token(TokenType.COLON, ":")
            case "'":
                tok = Token(TokenType.CHAR, self.read_char_literal())
            case '"':
                tok = Token(TokenType.STRING, self.read_string())
            case ",":
                tok = Token(TokenType.COMMA, ",")
            case ".":
                tok = Token(TokenType.PERIOD, ".")
            case "<":
                tok = Token(TokenType.LESSTHAN, "<")
            case ">":
                tok = Token(TokenType.GREATERTHAN, ">")
            case "/":
                tok = Token(TokenType.SLASH, "/")
            case "?":
                tok = Token(TokenType.QUESTION, "?")
            case "\\":
                tok = Token(TokenType.BACKSLASH, "\\")
            case "|":
                tok = Token(TokenType.PIPE, "|")
            case _:
                if self.ch.isalpha():
                    if ttype := IDENTIFIER_MAP.get((ident := self.read_identifier())):
                        return Token(ttype, ident)
                    else:
                        return Token(TokenType.IDENTIFIER, ident)
                elif self.ch.isdigit():
                    return Token(*self.read_number())
                return Token(TokenType.ILLEGAL, "ILLEGAL")

        self.advance()
        return tok

    def read_number(self) -> tuple[TokenType, str]:
        pos = self.curr
        is_float = False
        while self.ch is not None and self.ch.isdigit() or self.ch == ".":
            if self.ch == ".":
                if is_float:
                    return TokenType.ILLEGAL, "ILLEGAL"
                is_float = True
            self.advance()
        if is_float:
            return TokenType.FLOAT, self.input[pos : self.curr]
        return TokenType.NUMBER, self.input[pos : self.curr]

    def read_identifier(self) -> str:
        pos = self.curr
        while self.ch is not None and self.ch.isalnum() or self.ch == "_":
            self.advance()
        return self.input[pos : self.curr]

    def read_string(self) -> str:
        self.advance()
        pos = self.curr
        while self.ch is not None and self.ch != '"':
            self.advance()
        return self.input[pos : self.curr]

    def read_char_literal(self) -> str:
        self.advance()
        pos = self.curr
        while self.ch is not None and self.ch != "'":
            self.advance()
        return self.input[pos : self.curr]

    def read_comment(self) -> str:
        pos = self.curr
        while self.ch is not None and self.ch not in ["\n", "\r"]:
            self.advance()
        return self.input[pos : self.curr]

    def advance(self) -> None:
        if self.peek >= len(self.input):
            self.ch = None
        else:
            self.ch = self.input[self.peek]
        self.curr = self.peek
        self.peek += 1
