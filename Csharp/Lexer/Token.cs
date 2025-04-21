public enum TokenType {
    EOF,
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
}

public class TokenMap : Dictionary<string, TokenType>;

public class Token {
    public string literal;
    public TokenType type;

    public static TokenMap IDENTIFIER_MAP = new TokenMap {
        {"let",    TokenType.LET     },
        {"fn",     TokenType.FUNCTION},
        {"if",     TokenType.IF      },
        {"else",   TokenType.ELSE    },
        {"return", TokenType.RETURN  },

        {"true",   TokenType.BOOLEAN },
        {"false",  TokenType.BOOLEAN },

        {"bool",   TokenType.BOOL    },
        {"int",    TokenType.INT     },
    };

    public Token(TokenType type, string literal) {
        this.type    = type;
        this.literal = literal;
    }
}
