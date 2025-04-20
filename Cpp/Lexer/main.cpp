#include "lexer.hpp"

#include <cstdio>

int main() {
    string input = "fn is_gt_ten(num: int) bool {"
                   "    if (num > 10) {"
                   "        return true;"
                   "    } else {"
                   "        return false;"
                   "    }"
                   "}"
                   "let ten = 5 + 5 * 4 / 2 - 5;"
                   "print(is_big(ten));";

    Lexer lex = Lexer(input);
    Token tok = lex.nextToken();
    while (tok.type != ::_EOF) {
        printf("Token{ literal: \"%s\", type: %d }\n", tok.literal.c_str(), tok.type);
        tok = lex.nextToken();
    }
}
