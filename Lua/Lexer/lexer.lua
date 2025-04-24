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
local DoubleTokenMap = token.DoubleTokenMap

---@return Token
function Lexer:nextToken()
    while self.char:match("%s") do
        self:readChar()
    end

    if self.peek < self.input:len() then
        local ds = self.input:sub(self.curr, self.peek)
        if ds == "//" then return newToken(TokenType.COMMENT, self:readComment()) end

        local ttype = DoubleTokenMap[ds]
        if ttype ~= nil then
            self:readChar()
            self:readChar()
            return newToken(ttype, ds)
        end
    end

    local tok
    local c = self.char
    if c == "\0" then
        return newToken(TokenType.EOF, "\0")
    elseif c == "!" then
        tok = newToken(TokenType.EXCLAMATION, "!")
    elseif c == "@" then
        tok = newToken(TokenType.AT, "@")
    elseif c == "#" then
        tok = newToken(TokenType.HASHTAG, "#")
    elseif c == "$" then
        tok = newToken(TokenType.DOLLAR, "$")
    elseif c == "%" then
        tok = newToken(TokenType.PERCENT, "%")
    elseif c == "^" then
        tok = newToken(TokenType.CARET, "^")
    elseif c == "&" then
        tok = newToken(TokenType.AMPERSAND, "&")
    elseif c == "*" then
        tok = newToken(TokenType.ASTERISK, "*")
    elseif c == "(" then
        tok = newToken(TokenType.LPAREN, "(")
    elseif c == ")" then
        tok = newToken(TokenType.RPAREN, ")")
    elseif c == "-" then
        tok = newToken(TokenType.MINUS, "-")
    elseif c == "_" then
        tok = newToken(TokenType.UNDERSCORE, "_")
    elseif c == "+" then
        tok = newToken(TokenType.PLUS, "+")
    elseif c == "=" then
        tok = newToken(TokenType.ASSIGN, "=")
    elseif c == "[" then
        tok = newToken(TokenType.LBRACKET, "[")
    elseif c == "]" then
        tok = newToken(TokenType.RBRACKET, "]")
    elseif c == "{" then
        tok = newToken(TokenType.LBRACE, "{")
    elseif c == "}" then
        tok = newToken(TokenType.RBRACE, "}")
    elseif c == ";" then
        tok = newToken(TokenType.SEMICOLON, ";")
    elseif c == ":" then
        tok = newToken(TokenType.COLON, ":")
    elseif c == "'" then
        tok = newToken(TokenType.CHAR, self:readCharLiteral())
    elseif c == '"' then
        tok = newToken(TokenType.STRING, self:readString())
    elseif c == "," then
        tok = newToken(TokenType.COMMA, ",")
    elseif c == "." then
        tok = newToken(TokenType.PERIOD, ".")
    elseif c == "<" then
        tok = newToken(TokenType.LESSTHAN, "<")
    elseif c == ">" then
        tok = newToken(TokenType.GREATERTHAN, ">")
    elseif c == "/" then
        tok = newToken(TokenType.SLASH, "/")
    elseif c == "?" then
        tok = newToken(TokenType.QUESTION, "?")
    elseif c == "\\" then
        tok = newToken(TokenType.BACKSLASH, "\\")
    elseif c == "|" then
        tok = newToken(TokenType.PIPE, "|")
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
        tok = newToken(TokenType.ILLEGAL, "ILLEGAL")
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

---@return TokenType, string
function Lexer:readNumber()
    local pos = self.curr
    local is_float = false

    while self.char:match("[0-9.]") do
        if self.char == "." then
            if is_float then
                self:readChar()
                return TokenType.ILLEGAL, "ILLEGAL"
            end
            is_float = true
        end
        self:readChar()
    end

    if is_float then
        return TokenType.FLOAT, self.input:sub(pos, self.curr - 1)
    else
        return TokenType.NUMBER, self.input:sub(pos, self.curr - 1)
    end
end

---@return string
function Lexer:readString()
    self:readChar()
    local pos = self.curr
    while not self.char:match('["\0]') do
        self:readChar()
    end
    return self.input:sub(pos, self.curr - 1)
end

---@return string
function Lexer:readCharLiteral()
    self:readChar()
    local pos = self.curr
    while not self.char:match("['\0]") do
        self:readChar()
    end
    return self.input:sub(pos, self.curr - 1)
end
---@return string
function Lexer:readComment()
    local pos = self.curr
    while not self.char:match("[\n\r\0]") do
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
