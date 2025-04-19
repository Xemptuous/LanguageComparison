from tokens import IDENTIFIER_MAP, Token, TokenType


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
        self.read_char()

    def next_token(self) -> Token:
        tok: Token = Token(TokenType.EOF, "")

        while self.ch is not None and self.ch.isspace():
            self.read_char()

        if not self.ch:
            return tok

        match self.ch:
            case ";":
                tok = Token(TokenType.SEMICOLON, ";")
            case "*":
                tok = Token(TokenType.ASTERISK, "*")
            case "/":
                tok = Token(TokenType.SLASH, "/")
            case "-":
                tok = Token(TokenType.DASH, "-")
            case "+":
                tok = Token(TokenType.PLUS, "+")
            case "=":
                tok = Token(TokenType.EQUAL, "=")
            case "<":
                tok = Token(TokenType.LESSTHAN, "<")
            case ">":
                tok = Token(TokenType.GREATERTHAN, ">")
            case ";":
                tok = Token(TokenType.SEMICOLON, ";")
            case ":":
                tok = Token(TokenType.COLON, ":")
            case "(":
                tok = Token(TokenType.LPAREN, "(")
            case ")":
                tok = Token(TokenType.RPAREN, ")")
            case "{":
                tok = Token(TokenType.LBRACE, "{")
            case "}":
                tok = Token(TokenType.RBRACE, "}")
            case _:
                if self.ch.isalpha():
                    if ttype := IDENTIFIER_MAP.get((ident := self.read_identifier())):
                        return Token(ttype, ident)
                    else:
                        return Token(TokenType.IDENTIFIER, ident)
                elif self.ch.isdigit():
                    return Token(TokenType.NUMBER, self.read_number())
                return Token(TokenType.ILLEGAL, "ILLEGAL")

        self.read_char()
        return tok

    def read_identifier(self) -> str:
        pos = self.curr
        while self.ch is not None and self.ch.isalpha() or self.ch == "_":
            self.read_char()
        return self.input[pos : self.curr]

    def read_number(self) -> str:
        pos = self.curr
        while self.ch is not None and self.ch.isdigit():
            self.read_char()
        return self.input[pos : self.curr]

    def read_char(self) -> None:
        if self.peek >= len(self.input):
            self.ch = None
        else:
            self.ch = self.input[self.peek]
        self.curr = self.peek
        self.peek += 1
