import std/tables

type TokenType* = enum
    Eof = "EOF",
    Illegal = "ILLEGAL",

    # Core
    Let = "LET",
    Const = "CONST",
    Struct = "STRUCT",
    Function = "FUNCTION",
    If = "IF",
    Else = "ELSE",
    Switch = "SWITCH",
    Case = "CASE",
    Break = "BREAK",
    Return = "RETURN",
    While = "WHILE",
    For = "FOR",
    And = "AND",
    Or = "OR",
    In = "IN",

    # Types
    Identifier = "IDENTIFIER",
    Number = "NUMBER",
    Float = "FLOAT",
    Boolean = "BOOLEAN",
    String = "STRING",
    Char = "CHAR",

    # Datatypes
    Int = "INT",
    F32 = "F32",
    Bool = "BOOL",
    Str = "STR",
    Nil = "NIL",
    Void = "VOID",

    # Single Characters
    Exclamation = "EXCLAMATION", # !
    At = "AT",          # @
    Hashtag = "HASHTAG",     # #
    Dollar = "DOLLAR",      # $
    Percent = "PERCENT",     # %
    Caret = "CARET",       # ^
    Ampersand = "AMPERSAND",   # &
    Asterisk = "ASTERISK",    # *
    Lparen = "LPAREN",      # (
    Rparen = "RPAREN",      # )
    Minus = "MINUS",       # -
    Underscore = "UNDERSCORE",  # _
    Plus = "PLUS",        # +
    Assign = "ASSIGN",      # =
    Lbracket = "LBRACKET",    # [
    Rbracket = "RBRACKET",    # ]
    Lbrace = "LBRACE",      # {
    Rbrace = "RBRACE",      # }
    Semicolon = "SEMICOLON",   # ;
    Colon = "COLON",       # :
    Apostrophe = "APOSTROPHE",  # '
    Quote = "QUOTE",       # "
    Comma = "COMMA",       # ,
    Period = "PERIOD",      # .
    Lessthan = "LESSTHAN",    # <
    Greaterthan = "GREATERTHAN", # >
    Slash = "SLASH",       # /
    Question = "QUESTION",    # ?
    Backslash = "BACKSLASH",   # \/
    Pipe = "PIPE",        # |

    # Double Characters
    Equal = "EQUAL",     # ==
    NotEqual = "NOT_EQUAL", # !=
    PlusEq = "PLUS_EQ",   # +=
    MinusEq = "MINUS_EQ",  # -=
    MultEq = "MULT_EQ",   # *=
    DivEq = "DIV_EQ",    # /=
    LtEqual = "LT_EQUAL",  # <=
    GtEqual = "GT_EQUAL",  # >=
    Increment = "INCREMENT", # ++
    Decrement = "DECREMENT", # --
    Comment = "COMMENT",   # //



type Token* = object
    token_type*: TokenType
    literal*: string

proc newToken*(token_type: TokenType, literal: string): Token =
    return Token(token_type: token_type, literal: literal)

const IDENTIFIER_MAP* = {
    "let": TokenType.Let,
    "const": TokenType.Const,
    "struct": TokenType.Struct,
    "fn": TokenType.Function,
    "if": TokenType.If,
    "else": TokenType.Else,
    "switch": TokenType.Switch,
    "case": TokenType.Case,
    "break": TokenType.Break,
    "return": TokenType.Return,
    "while": TokenType.While,
    "for": TokenType.For,
    "and": TokenType.And,
    "or": TokenType.Or,
    "in": TokenType.In,

    "true": TokenType.Boolean,
    "false": TokenType.Boolean,

    "bool": TokenType.Bool,
    "int": TokenType.Int,
    "f32": TokenType.F32,
    "str": TokenType.Str,
    "nil": TokenType.Nil,
    "void": TokenType.Void,
}.toTable()

const DOUBLE_TOKEN_MAP* = {
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
}.toTable()
