---@class Lexer
---@field input string
---@field char string
---@field curr number
---@field peek number
local Lexer = {
    input = "",
    char = "",
    curr = 1,
    peek = 1,
}

local token = require("token")
local newToken = token.newToken
local TokenType = token.TokenType
local IdentifierMap = token.IdentifierMap

---@return Token
function Lexer:nextToken()
    while self.char:match("%s") do
        self:readChar()
    end

    local tok
    local c = self.char
    if c == "\0" then
        tok = newToken(TokenType.EOF, "\0")
    elseif c == "*" then
        tok = newToken(TokenType.ASTERISK, "*")
    elseif c == "/" then
        tok = newToken(TokenType.SLASH, "/")
    elseif c == "-" then
        tok = newToken(TokenType.DASH, "-")
    elseif c == "+" then
        tok = newToken(TokenType.PLUS, "+")
    elseif c == "=" then
        tok = newToken(TokenType.EQUAL, "=")
    elseif c == "<" then
        tok = newToken(TokenType.LESSTHAN, "<")
    elseif c == ">" then
        tok = newToken(TokenType.GREATERTHAN, ">")
    elseif c == ";" then
        tok = newToken(TokenType.SEMICOLON, ";")
    elseif c == ":" then
        tok = newToken(TokenType.COLON, ":")
    elseif c == "(" then
        tok = newToken(TokenType.LPAREN, "(")
    elseif c == ")" then
        tok = newToken(TokenType.RPAREN, ")")
    elseif c == "{" then
        tok = newToken(TokenType.LBRACE, "{")
    elseif c == "}" then
        tok = newToken(TokenType.RBRACE, "}")
    else
        if self.char:match("[a-zA-Z+]") then
            local ident = self:readIdentifier()
            local ttype = IdentifierMap[ident]
            if ttype ~= nil then
                return newToken(ttype, ident)
            else
                return newToken(TokenType.IDENTIFIER, ident)
            end
        elseif self.char:match("[0-9]") then
            return newToken(TokenType.NUMBER, self:readNumber())
        end
        return newToken(TokenType.ILLEGAL, "ILLEGAL")
    end
    self:readChar()

    return tok
end

---@return string
function Lexer:readIdentifier()
    local pos = self.curr
    while self.char:match("[a-zA-Z_]") do
        self:readChar()
    end
    return self.input:sub(pos, self.curr - 1)
end

---@return string
function Lexer:readNumber()
    local pos = self.curr
    while self.char:match("[0-9]") do
        self:readChar()
    end
    return self.input:sub(pos, self.curr - 1)
end

---@return nil
function Lexer:readChar()
    if self.peek > self.input:len() then
        self.char = "\0"
    else
        self.char = self.input:sub(self.peek, self.peek)
    end
    self.curr = self.peek
    self.peek = self.peek + 1
end

return Lexer
