#include <string>
#include <unordered_map>

using namespace std;

enum TokenType {
    _EOF,
    ILLEGAL,

    LET,
    FUNCTION,
    IF,
    ELSE,
    RETURN,

    IDENTIFIER,
    NUMBER,
    BOOLEAN,

    INT,
    BOOL,

    // Single Characters
    ASTERISK,    // *
    SLASH,       // /
    DASH,        // -
    PLUS,        // +
    EQUAL,       // =
    LESSTHAN,    // <
    GREATERTHAN, // >
    SEMICOLON,   // ;
    COLON,       // :
    LPAREN,      // (
    RPAREN,      // )
    LBRACE,      // {
    RBRACE,      // }
};

struct Token {
    string literal;
    TokenType type;

    Token(TokenType type, string literal) : type(type), literal(literal) {};
};

static const unordered_map<string, TokenType> IDENTIFIER_MAP{
    {"let",    ::LET     },
    {"fn",     ::FUNCTION},
    {"if",     ::IF      },
    {"else",   ::ELSE    },
    {"return", ::RETURN  },

    {"true",   ::BOOLEAN },
    {"false",  ::BOOLEAN },

    {"bool",   ::BOOL    },
    {"int",    ::INT     },
};
