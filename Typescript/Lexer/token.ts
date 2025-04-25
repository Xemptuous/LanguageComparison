export enum TokenType {
    EOF = "EOF",
    ILLEGAL = "ILLEGAL",

    // Core
    LET = "LET",
    CONST = "CONST",
    STRUCT = "STRUCT",
    FUNCTION = "FUNCTION",
    IF = "IF",
    ELSE = "ELSE",
    SWITCH = "SWITCH",
    CASE = "CASE",
    BREAK = "BREAK",
    RETURN = "RETURN",
    WHILE = "WHILE",
    FOR = "FOR",
    AND = "AND",
    OR = "OR",
    IN = "IN",

    // Types
    IDENTIFIER = "IDENTIFIER",
    NUMBER = "NUMBER",
    FLOAT = "FLOAT",
    BOOLEAN = "BOOLEAN",
    STRING = "STRING",
    CHAR = "CHAR",

    // Datatypes
    INT = "INT",
    F32 = "F32",
    BOOL = "BOOL",
    STR = "STR",
    NIL = "NIL",
    VOID = "VOID",

    // Single Characters
    EXCLAMATION = "EXCLAMATION", // !
    AT = "AT",          // @
    HASHTAG = "HASHTAG",     // #
    DOLLAR = "DOLLAR",      // $
    PERCENT = "PERCENT",     // %
    CARET = "CARET",       // ^
    AMPERSAND = "AMPERSAND",   // &
    ASTERISK = "ASTERISK",    // *
    LPAREN = "LPAREN",      // (
    RPAREN = "RPAREN",      // )
    MINUS = "MINUS",       // -
    UNDERSCORE = "UNDERSCORE",  // _
    PLUS = "PLUS",        // +
    ASSIGN = "ASSIGN",      // =
    LBRACKET = "LBRACKET",    // [
    RBRACKET = "RBRACKET",    // ]
    LBRACE = "LBRACE",      // {
    RBRACE = "RBRACE",      // }
    SEMICOLON = "SEMICOLON",   // ;
    COLON = "COLON",       // :
    APOSTROPHE = "APOSTROPHE",  // '
    QUOTE = "QUOTE",       // "
    COMMA = "COMMA",       // ,
    PERIOD = "PERIOD",      // .
    LESSTHAN = "LESSTHAN",    // <
    GREATERTHAN = "GREATERTHAN", // >
    SLASH = "SLASH",       // /
    QUESTION = "QUESTION",    // ?
    BACKSLASH = "BACKSLASH",   // \/
    PIPE = "PIPE",        // |

    // Double Characters
    EQUAL = "EQUAL",     // ==
    NOTEQUAL = "NOT_EQUAL", // !=
    PLUSEQ = "PLUS_EQ",   // +=
    MINUSEQ = "MINUS_EQ",  // -=
    MULTEQ = "MULT_EQ",   // *=
    DIVEQ = "DIV_EQ",    // /=
    LTEQUAL = "LT_EQUAL",  // <=
    GTEQUAL = "GT_EQUAL",  // >=
    INCREMENT = "INCREMENT", // ++
    DECREMENT = "DECREMENT", // --
    COMMENT = "COMMENT",   // //
}

export type Token = {
  type: TokenType;
  literal: string;
};

export function newToken(type: TokenType, literal: string): Token {
  return { literal: literal, type: type };
}

export const IDENTIFIER_MAP = {
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

export const DOUBLE_TOKEN_MAP = {
    "==": TokenType.EQUAL,
    "!=": TokenType.NOTEQUAL,
    "+=": TokenType.PLUSEQ,
    "-=": TokenType.MINUSEQ,
    "*=": TokenType.MULTEQ,
    "/=": TokenType.DIVEQ,
    "<=": TokenType.LTEQUAL,
    ">=": TokenType.GTEQUAL,
    "++": TokenType.INCREMENT,
    "--": TokenType.DECREMENT,
    "//": TokenType.COMMENT,
}
