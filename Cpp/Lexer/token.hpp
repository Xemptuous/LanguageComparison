#include <cstdio>
#include <string>
#include <unordered_map>

using namespace std;

enum TokenType {
    _EOF,
    ILLEGAL,

    // Core
    LET,
    CONST,
    STRUCT,
    FUNCTION,
    IF,
    ELSE,
    SWITCH,
    CASE,
    BREAK,
    RETURN,
    WHILE,
    FOR,
    AND,
    OR,
    IN,

    // Types
    IDENTIFIER,
    NUMBER,
    FLOAT,
    BOOLEAN,
    STRING,
    CHAR,

    // Datatypes
    INT,
    F32,
    BOOL,
    STR,
    NIL,
    VOID,

    // Single Characters
    EXCLAMATION, // !
    AT,          // @
    HASHTAG,     // #
    DOLLAR,      // $
    PERCENT,     // %
    CARET,       // ^
    AMPERSAND,   // &
    ASTERISK,    // *
    LPAREN,      // (
    RPAREN,      // )
    MINUS,       // -
    UNDERSCORE,  // _
    PLUS,        // +
    ASSIGN,      // =
    LBRACKET,    // [
    RBRACKET,    // ]
    LBRACE,      // {
    RBRACE,      // }
    SEMICOLON,   // ;
    COLON,       // :
    APOSTROPHE,  // '
    QUOTE,       // "
    COMMA,       // ,
    PERIOD,      // .
    LESSTHAN,    // <
    GREATERTHAN, // >
    SLASH,       // /
    QUESTION,    // ?
    BACKSLASH,   // \/
    PIPE,        // |

    // Double Characters
    EQUAL,     // ==
    NOT_EQUAL, // !=
    PLUS_EQ,   // +=
    MINUS_EQ,  // -=
    MULT_EQ,   // *=
    DIV_EQ,    // /=
    LT_EQUAL,  // <=
    GT_EQUAL,  // >=
    INCREMENT, // ++
    DECREMENT, // --
    COMMENT,   // //
};

struct Token {
    string literal;
    TokenType type;

    Token(TokenType type, string literal) : type(type), literal(literal) {};
    inline void print() {
        string type = (const char*[]){
            "EOF",
            "ILLEGAL",

            // Core
            "LET",
            "CONST",
            "STRUCT",
            "FUNCTION",
            "IF",
            "ELSE",
            "SWITCH",
            "CASE",
            "BREAK",
            "RETURN",
            "WHILE",
            "FOR",
            "AND",
            "OR",
            "IN",

            // Types
            "IDENTIFIER",
            "NUMBER",
            "FLOAT",
            "BOOLEAN",
            "STRING",
            "CHAR",

            // Datatypes
            "INT",
            "F32",
            "BOOL",
            "STR",
            "NIL",
            "VOID",

            // Single Characters
            "EXCLAMATION",
            "AT",
            "HASHTAG",
            "DOLLAR",
            "PERCENT",
            "CARET",
            "AMPERSAND",
            "ASTERISK",
            "LPAREN",
            "RPAREN",
            "MINUS",
            "UNDERSCORE",
            "PLUS",
            "ASSIGN",
            "LBRACKET",
            "RBRACKET",
            "LBRACE",
            "RBRACE",
            "SEMICOLON",
            "COLON",
            "APOSTROPHE",
            "QUOTE",
            "COMMA",
            "PERIOD",
            "LESSTHAN",
            "GREATERTHAN",
            "SLASH",
            "QUESTION",
            "BACKSLASH",
            "PIPE",

            // Double Characters
            "EQUAL",
            "NOT_EQUAL",
            "PLUS_EQ",
            "MINUS_EQ",
            "MULT_EQ",
            "DIV_EQ",
            "LT_EQUAL",
            "GT_EQUAL",
            "INCREMENT",
            "DECREMENT",
            "COMMENT",
        }[this->type];
        printf("Token literal: \"%s\" type: %s\n", this->literal.c_str(), type.c_str());
    };
};

static const unordered_map<string, TokenType> IDENTIFIER_MAP{
    {"let",    ::LET     },
    {"const",  ::CONST   },
    {"struct", ::STRUCT  },
    {"fn",     ::FUNCTION},
    {"if",     ::IF      },
    {"else",   ::ELSE    },
    {"switch", ::SWITCH  },
    {"case",   ::CASE    },
    {"break",  ::BREAK   },
    {"return", ::RETURN  },
    {"while",  ::WHILE   },
    {"for",    ::FOR     },
    {"and",    ::AND     },
    {"or",     ::OR      },
    {"in",     ::IN      },

    {"true",   ::BOOLEAN },
    {"false",  ::BOOLEAN },

    {"bool",   ::BOOL    },
    {"int",    ::INT     },
    {"f32",    ::F32     },
    {"str",    ::STR     },
    {"nil",    ::NIL     },
    {"void",   ::VOID    },
};

static const unordered_map<string, TokenType> DOUBLE_TOKEN_MAP{
    {"==", ::EQUAL    },
    {"!=", ::NOT_EQUAL},
    {"+=", ::PLUS_EQ  },
    {"-=", ::MINUS_EQ },
    {"*=", ::MULT_EQ  },
    {"/=", ::DIV_EQ   },
    {"<=", ::LT_EQUAL },
    {">=", ::GT_EQUAL },
    {"++", ::INCREMENT},
    {"--", ::DECREMENT},
    {"//", ::COMMENT  },
};
