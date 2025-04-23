public enum TokenType {
    EOF,
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
}

public class TokenMap : Dictionary<string, TokenType>;

public class Token {
    public string literal;
    public TokenType type;

    public Token(TokenType type, string literal) {
        this.type    = type;
        this.literal = literal;
    }

    public static TokenMap IDENTIFIER_MAP = new TokenMap {
        {"let",    TokenType.LET     },
        {"const",  TokenType.CONST   },
        {"struct", TokenType.STRUCT  },
        {"fn",     TokenType.FUNCTION},
        {"if",     TokenType.IF      },
        {"else",   TokenType.ELSE    },
        {"switch", TokenType.SWITCH  },
        {"case",   TokenType.CASE    },
        {"break",  TokenType.BREAK   },
        {"return", TokenType.RETURN  },
        {"while",  TokenType.WHILE   },
        {"for",    TokenType.FOR     },
        {"and",    TokenType.AND     },
        {"or",     TokenType.OR      },
        {"in",     TokenType.IN      },

        {"true",   TokenType.BOOLEAN },
        {"false",  TokenType.BOOLEAN },

        {"bool",   TokenType.BOOL    },
        {"int",    TokenType.INT     },
        {"f32",    TokenType.F32     },
        {"str",    TokenType.STR     },
        {"nil",    TokenType.NIL     },
        {"void",   TokenType.VOID    },
    };

    public static TokenMap DOUBLE_TOKEN_MAP = new TokenMap {
        {"==", TokenType.EQUAL    },
        {"!=", TokenType.NOT_EQUAL},
        {"+=", TokenType.PLUS_EQ  },
        {"-=", TokenType.MINUS_EQ },
        {"*=", TokenType.MULT_EQ  },
        {"/=", TokenType.DIV_EQ   },
        {"<=", TokenType.LT_EQUAL },
        {">=", TokenType.GT_EQUAL },
        {"++", TokenType.INCREMENT},
        {"--", TokenType.DECREMENT},
        {"//", TokenType.COMMENT  },
    };
}
