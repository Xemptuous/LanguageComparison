#include "token.hpp"

#include <cstdint>
#include <string>

using namespace std;

struct Lexer {
    string input;
    uint64_t curr;
    uint64_t peek;
    char ch;

    Lexer(string input);

    Token nextToken();
    string readIdentifier();
    string readNumber();
    void readChar();
};
