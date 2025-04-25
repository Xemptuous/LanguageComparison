from enum import Enum, auto


class TokenType(Enum):
    EOF = auto()
    ILLEGAL = auto()

    # Core
    LET = auto()
    CONST = auto()
    STRUCT = auto()
    FUNCTION = auto()
    IF = auto()
    ELSE = auto()
    SWITCH = auto()
    CASE = auto()
    BREAK = auto()
    RETURN = auto()
    WHILE = auto()
    FOR = auto()
    AND = auto()
    OR = auto()
    IN = auto()

    # Types
    IDENTIFIER = auto()
    NUMBER = auto()
    FLOAT = auto()
    BOOLEAN = auto()
    STRING = auto()
    CHAR = auto()

    # Datatypes
    INT = auto()
    F32 = auto()
    BOOL = auto()
    STR = auto()
    NIL = auto()
    VOID = auto()

    # Single Characters
    EXCLAMATION = auto()  # !
    AT = auto()  # @
    HASHTAG = auto()  # #
    DOLLAR = auto()  # $
    PERCENT = auto()  # %
    CARET = auto()  # ^
    AMPERSAND = auto()  # &
    ASTERISK = auto()  # *
    LPAREN = auto()  # (
    RPAREN = auto()  # )
    MINUS = auto()  # -
    UNDERSCORE = auto()  # _
    PLUS = auto()  # +
    ASSIGN = auto()  # =
    LBRACKET = auto()  # [
    RBRACKET = auto()  # ]
    LBRACE = auto()  # {
    RBRACE = auto()  # }
    SEMICOLON = auto()  # ;
    COLON = auto()  # :
    APOSTROPHE = auto()  # '
    QUOTE = auto()  # "
    COMMA = auto()  # ,
    PERIOD = auto()  # .
    LESSTHAN = auto()  # <
    GREATERTHAN = auto()  # >
    SLASH = auto()  # /
    QUESTION = auto()  # ?
    BACKSLASH = auto()  # \/
    PIPE = auto()  # |

    # Double Characters
    EQUAL = auto()  # ==
    NOT_EQUAL = auto()  # !=
    PLUS_EQ = auto()  # +=
    MINUS_EQ = auto()  # -=
    MULT_EQ = auto()  # *=
    DIV_EQ = auto()  # /=
    LT_EQUAL = auto()  # <=
    GT_EQUAL = auto()  # >=
    INCREMENT = auto()  # ++
    DECREMENT = auto()  # --
    COMMENT = auto()  # //


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
    "const": TokenType.CONST,
    "struct": TokenType.STRUCT,
    "fn": TokenType.FUNCTION,
    "if": TokenType.IF,
    "else": TokenType.ELSE,
    "switch": TokenType.SWITCH,
    "case": TokenType.CASE,
    "break": TokenType.BREAK,
    "return": TokenType.RETURN,
    "while": TokenType.WHILE,
    "for": TokenType.FOR,
    "and": TokenType.AND,
    "or": TokenType.OR,
    "in": TokenType.IN,
    "true": TokenType.BOOLEAN,
    "false": TokenType.BOOLEAN,
    "bool": TokenType.BOOL,
    "int": TokenType.INT,
    "f32": TokenType.F32,
    "str": TokenType.STR,
    "nil": TokenType.NIL,
    "void": TokenType.VOID,
}

DOUBLE_TOKEN_MAP = {
    "==": TokenType.EQUAL,
    "!=": TokenType.NOT_EQUAL,
    "+=": TokenType.PLUS_EQ,
    "-=": TokenType.MINUS_EQ,
    "*=": TokenType.MULT_EQ,
    "/=": TokenType.DIV_EQ,
    "<=": TokenType.LT_EQUAL,
    ">=": TokenType.GT_EQUAL,
    "++": TokenType.INCREMENT,
    "--": TokenType.DECREMENT,
    "//": TokenType.COMMENT,
}
