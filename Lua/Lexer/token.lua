---@class Token
---@field literal string?
---@field type string?
local Token = {
    literal = nil,
    type = nil,
}

---@enum TokenType
local TokenType = {
    EOF = "EOF",
    ILLEGAL = "ILLEGAL",

    -- Core
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

    -- Types
    IDENTIFIER = "IDENTIFIER",
    NUMBER = "NUMBER",
    FLOAT = "FLOAT",
    BOOLEAN = "BOOLEAN",
    STRING = "STRING",
    CHAR = "CHAR",

    -- Datatypes
    INT = "INT",
    F32 = "F32",
    BOOL = "BOOL",
    STR = "STR",
    NIL = "NIL",
    VOID = "VOID",

    -- Single Characters
    EXCLAMATION = "EXCLAMATION", -- !
    AT = "AT", -- @
    HASHTAG = "HASHTAG", -- #
    DOLLAR = "DOLLAR", -- $
    PERCENT = "PERCENT", -- %
    CARET = "CARET", -- ^
    AMPERSAND = "AMPERSAND", -- &
    ASTERISK = "ASTERISK", -- *
    LPAREN = "LPAREN", -- (
    RPAREN = "RPAREN", -- )
    MINUS = "MINUS", -- -
    UNDERSCORE = "UNDERSCORE", -- _
    PLUS = "PLUS", -- +
    ASSIGN = "ASSIGN", -- =
    LBRACKET = "LBRACKET", -- [
    RBRACKET = "RBRACKET", -- ]
    LBRACE = "LBRACE", -- {
    RBRACE = "RBRACE", -- }
    SEMICOLON = "SEMICOLON", -- ;
    COLON = "COLON", -- :
    APOSTROPHE = "APOSTROPHE", -- '
    QUOTE = "QUOTE", -- "
    COMMA = "COMMA", -- ,
    PERIOD = "PERIOD", -- .
    LESSTHAN = "LESSTHAN", -- <
    GREATERTHAN = "GREATERTHAN", -- >
    SLASH = "SLASH", -- /
    QUESTION = "QUESTION", -- ?
    BACKSLASH = "BACKSLASH", -- \/
    PIPE = "PIPE", -- |

    -- Double Characters
    EQUAL = "EQUAL", -- ==
    NOT_EQUAL = "NOT_EQUAL", -- !=
    PLUS_EQ = "PLUS_EQ", -- +=
    MINUS_EQ = "MINUS_EQ", -- -=
    MULT_EQ = "MULT_EQ", -- *=
    DIV_EQ = "DIV_EQ", -- /=
    LT_EQUAL = "LT_EQUAL", -- <=
    GT_EQUAL = "GT_EQUAL", -- >=
    INCREMENT = "INCREMENT", -- ++
    DECREMENT = "DECREMENT", -- --
    COMMENT = "COMMENT", -- //
}

---@param type TokenType
---@param literal string
---@return Token
local function newToken(type, literal)
    return {
        type = type,
        literal = literal,
    }
end

---@type {[string]: TokenType}
local IdentifierMap = {
    ["let"] = TokenType.LET,
    ["const"] = TokenType.CONST,
    ["struct"] = TokenType.STRUCT,
    ["fn"] = TokenType.FUNCTION,
    ["if"] = TokenType.IF,
    ["else"] = TokenType.ELSE,
    ["switch"] = TokenType.SWITCH,
    ["case"] = TokenType.CASE,
    ["break"] = TokenType.BREAK,
    ["return"] = TokenType.RETURN,
    ["while"] = TokenType.WHILE,
    ["for"] = TokenType.FOR,
    ["and"] = TokenType.AND,
    ["or"] = TokenType.OR,
    ["in"] = TokenType.IN,

    ["true"] = TokenType.BOOLEAN,
    ["false"] = TokenType.BOOLEAN,

    ["bool"] = TokenType.BOOL,
    ["int"] = TokenType.INT,
    ["f32"] = TokenType.F32,
    ["str"] = TokenType.STR,
    ["nil"] = TokenType.NIL,
    ["void"] = TokenType.VOID,
}

local DoubleTokenMap = {
    ["=="] = TokenType.EQUAL,
    ["!="] = TokenType.NOT_EQUAL,
    ["+="] = TokenType.PLUS_EQ,
    ["-="] = TokenType.MINUS_EQ,
    ["*="] = TokenType.MULT_EQ,
    ["/="] = TokenType.DIV_EQ,
    ["<="] = TokenType.LT_EQUAL,
    [">="] = TokenType.GT_EQUAL,
    ["++"] = TokenType.INCREMENT,
    ["--"] = TokenType.DECREMENT,
    ["//"] = TokenType.COMMENT,
}

return {
    Token = Token,
    TokenType = TokenType,
    IdentifierMap = IdentifierMap,
    DoubleTokenMap = DoubleTokenMap,
    newToken = newToken,
}
