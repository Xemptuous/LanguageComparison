import std/tables

type TokenType* = enum
    Eof = "EOF",
    Illegal = "ILLEGAL",

    Let = "LET",
    Function = "FUNCTION",
    If = "IF",
    Else = "ELSE",
    Return = "RETURN",

    Identifier = "IDENTIFIER",
    Number = "NUMBER",
    Boolean = "BOOLEAN",

    Int = "INT",
    Bool = "BOOL",

    Asterisk = "ASTERISK",
    Slash = "SLASH",
    Dash = "DASH",
    Plus = "PLUS",
    Equal = "EQUAL",
    Lessthan = "LESSTHAN",
    Greaterthan = "GREATERTHAN",
    Semicolon = "SEMICOLON",
    Colon = "COLON",
    Lparen = "LPAREN",
    Rparen = "RPAREN",
    Lbrace = "LBRACE",
    Rbrace = "RBRACE",

type Token* = object
    token_type*: TokenType
    literal*: string

proc newToken*(token_type: TokenType, literal: string): Token =
    return Token(token_type: token_type, literal: literal)

proc getIdentifierMap*(): Table[string, TokenType] =
    var map = initTable[string, TokenType]()
    map["let"] = TokenType.Let
    map["fn"] = TokenType.Function
    map["if"] = TokenType.If
    map["else"] = TokenType.Else
    map["return"] = TokenType.Return

    map["true"] = TokenType.Boolean
    map["false"] = TokenType.Boolean

    map["bool"] = TokenType.Bool
    map["int"] = TokenType.Int
    return map


const IDENTIFIER_MAP* = {
    "let": TokenType.Let,
    "fn": TokenType.Function,
    "if": TokenType.If,
    "else": TokenType.Else,
    "return": TokenType.Return,
    "true": TokenType.Boolean,
    "false": TokenType.Boolean,
    "bool": TokenType.Bool,
    "int": TokenType.Int
}.toTable()
