from enum import Enum, auto


class TokenType(Enum):
    EOF = auto()
    ILLEGAL = auto()

    LET = auto()
    FUNCTION = auto()
    IF = auto()
    ELSE = auto()
    RETURN = auto()

    IDENTIFIER = auto()
    NUMBER = auto()
    BOOLEAN = auto()

    INT = auto()
    BOOL = auto()

    ASTERISK = auto()
    SLASH = auto()
    DASH = auto()
    PLUS = auto()
    EQUAL = auto()
    LESSTHAN = auto()
    GREATERTHAN = auto()
    SEMICOLON = auto()
    COLON = auto()
    LPAREN = auto()
    RPAREN = auto()
    LBRACE = auto()
    RBRACE = auto()


class Token:
    literal: str
    token_type: TokenType

    def __init__(self, ttype: TokenType, literal: str) -> None:
        self.literal = literal
        self.token_type = ttype

    def __str__(self) -> str:
        return f'Token( literal: "{self.literal}", type: {self.token_type} )'


IDENTIFIER_MAP = {
    "let": TokenType.LET,
    "fn": TokenType.FUNCTION,
    "if": TokenType.IF,
    "else": TokenType.ELSE,
    "return": TokenType.RETURN,
    "true": TokenType.BOOLEAN,
    "false": TokenType.BOOLEAN,
    "bool": TokenType.BOOL,
    "int": TokenType.INT,
}
