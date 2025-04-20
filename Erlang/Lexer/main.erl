-module(main).

-import(lexer, [new/1, parse_while/1]).

-export([start/0]).

-compile(start).

start() ->
    Input =
        "fn is_gt_ten(num: int) bool {"
        ++ "    if (num > 10) {"
        ++ "        return true;"
        ++ "    } else {"
        ++ "        return false;"
        ++ "    }"
        ++ "}"
        ++ "let ten = 5 + 5 * 4 / 2 - 5;"
        ++ "print(is_big(ten));",
    L = lexer:new(Input),
    parse_while(L).
