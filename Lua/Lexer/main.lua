local token = require("token")
local lexer = require("lexer")

lexer.input = "fn is_gt_ten(num: int) bool {"
    .. "    if (num > 10) {"
    .. "        return true;"
    .. "    } else {"
    .. "        return false;"
    .. "    }"
    .. "}"
    .. "let ten = 5 + 5 * 4 / 2 - 5;"
    .. "print(is_big(ten));"

lexer:readChar()

local tok = lexer:nextToken()
while tok.type ~= token.TokenType.EOF do
    print('Token{ literal: "' .. tok.literal .. '" type: ' .. tok.type .. " }")
    tok = lexer:nextToken()
end
