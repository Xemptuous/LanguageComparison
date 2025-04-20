enum TokenType
{
    Eof,
    Illegal,
    Let,
    Function,
    If,
    Else,
    Return,
    Identifier,
    Number,
    Boolean,
    Int,
    Bool,
    Asterisk,
    Slash,
    Dash,
    Plus,
    Equal,
    Lessthan,
    Greaterthan,
    Semicolon,
    Colon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
}

struct Token
{
    TokenType type;
    string literal;
}

TokenType[string] IDENTIFIER_MAP = [
    "let": TokenType.Let,
    "fn": TokenType.Function,

    "let": TokenType.Let,
    "fn": TokenType.Function,
    "if": TokenType.If,
    "else": TokenType.Else,
    "return": TokenType.Return,

    "true": TokenType.Boolean,
    "false": TokenType.Boolean,

    "bool": TokenType.Bool,
    "int": TokenType.Int,
];
