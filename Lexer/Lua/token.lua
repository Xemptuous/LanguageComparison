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

    LET = "LET",
    FUNCTION = "FUNCTION",
    IF = "IF",
    ELSE = "ELSE",
    RETURN = "RETURN",

    IDENTIFIER = "IDENTIFIER",
    NUMBER = "NUMBER",
    BOOLEAN = "BOOLEAN",

    INT = "INT",
    BOOL = "BOOL",

    ASTERISK = "ASTERISK",
    SLASH = "SLASH",
    DASH = "DASH",
    PLUS = "PLUS",
    EQUAL = "EQUAL",
    LESSTHAN = "LESSTHAN",
    GREATERTHAN = "GREATERTHAN",
    SEMICOLON = "SEMICOLON",
    COLON = "COLON",
    LPAREN = "LPAREN",
    RPAREN = "RPAREN",
    LBRACE = "LBRACE",
    RBRACE = "RBRACE",
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
    ["fn"] = TokenType.FUNCTION,
    ["if"] = TokenType.IF,
    ["else"] = TokenType.ELSE,
    ["return"] = TokenType.RETURN,

    ["true"] = TokenType.BOOLEAN,
    ["false"] = TokenType.BOOLEAN,

    ["bool"] = TokenType.BOOL,
    ["int"] = TokenType.INT,
}

return {
    Token = Token,
    TokenType = TokenType,
    IdentifierMap = IdentifierMap,
    newToken = newToken,
}
