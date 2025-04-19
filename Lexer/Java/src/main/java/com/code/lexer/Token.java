package com.code.lexer;

import java.util.HashMap;

public class Token {
    public String literal;
    public TokenType token_type;
    public static final HashMap<String, TokenType> IDENTIFIER_MAP;

    Token(TokenType token_type, String literal) {
        this.literal = literal;
        this.token_type = token_type;
    }

    enum TokenType {
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
        ASTERISK, // *
        SLASH, // /
        DASH, // -
        PLUS, // +
        EQUAL, // =
        LESSTHAN, // <
        GREATERTHAN, // >
        SEMICOLON, // ;
        COLON, // :
        LPAREN, // (
        RPAREN, // )
        LBRACE, // {
        RBRACE, // }
    }

    static {
        IDENTIFIER_MAP = new HashMap<String, TokenType>();
        IDENTIFIER_MAP.put("let", TokenType.LET);
        IDENTIFIER_MAP.put("fn", TokenType.FUNCTION);
        IDENTIFIER_MAP.put("if", TokenType.IF);
        IDENTIFIER_MAP.put("else", TokenType.ELSE);
        IDENTIFIER_MAP.put("return", TokenType.RETURN);

        IDENTIFIER_MAP.put("true", TokenType.BOOLEAN);
        IDENTIFIER_MAP.put("false", TokenType.BOOLEAN);

        IDENTIFIER_MAP.put("bool", TokenType.BOOL);
        IDENTIFIER_MAP.put("int", TokenType.INT);
    }

    @Override
    public String toString() {
        return "Token {" + " literal: \"" + this.literal + "\" type: " + this.token_type + " }";
    }
}
