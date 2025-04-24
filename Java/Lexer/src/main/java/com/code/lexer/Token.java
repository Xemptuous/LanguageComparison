package com.code.lexer;

import java.util.HashMap;

public class Token {
    public String literal;
    public TokenType token_type;
    public static final HashMap<String, TokenType> IDENTIFIER_MAP = new HashMap<String, TokenType>() {
        {
            put("let", TokenType.LET);
            put("const", TokenType.CONST);
            put("struct", TokenType.STRUCT);
            put("fn", TokenType.FUNCTION);
            put("if", TokenType.IF);
            put("else", TokenType.ELSE);
            put("switch", TokenType.SWITCH);
            put("case", TokenType.CASE);
            put("break", TokenType.BREAK);
            put("return", TokenType.RETURN);
            put("while", TokenType.WHILE);
            put("for", TokenType.FOR);
            put("and", TokenType.AND);
            put("or", TokenType.OR);
            put("in", TokenType.IN);

            put("true", TokenType.BOOLEAN);
            put("false", TokenType.BOOLEAN);

            put("bool", TokenType.BOOL);
            put("int", TokenType.INT);
            put("f32", TokenType.F32);
            put("str", TokenType.STR);
            put("nil", TokenType.NIL);
            put("void", TokenType.VOID);

        }
    };
    public static final HashMap<String, TokenType> DOUBLE_TOKEN_MAP = new HashMap<String, TokenType>() {
        {
            put("==", TokenType.EQUAL);
            put("!=", TokenType.NOT_EQUAL);
            put("+=", TokenType.PLUS_EQ);
            put("-=", TokenType.MINUS_EQ);
            put("*=", TokenType.MULT_EQ);
            put("/=", TokenType.DIV_EQ);
            put("<=", TokenType.LT_EQUAL);
            put(">=", TokenType.GT_EQUAL);
            put("++", TokenType.INCREMENT);
            put("--", TokenType.DECREMENT);
            put("//", TokenType.COMMENT);
        }
    };

    Token(TokenType token_type, String literal) {
        this.literal = literal;
        this.token_type = token_type;
    }

    enum TokenType {
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
        AT, // @
        HASHTAG, // #
        DOLLAR, // $
        PERCENT, // %
        CARET, // ^
        AMPERSAND, // &
        ASTERISK, // *
        LPAREN, // (
        RPAREN, // )
        MINUS, // -
        UNDERSCORE, // _
        PLUS, // +
        ASSIGN, // =
        LBRACKET, // [
        RBRACKET, // ]
        LBRACE, // {
        RBRACE, // }
        SEMICOLON, // ;
        COLON, // :
        APOSTROPHE, // '
        QUOTE, // "
        COMMA, // ,
        PERIOD, // .
        LESSTHAN, // <
        GREATERTHAN, // >
        SLASH, // /
        QUESTION, // ?
        BACKSLASH, // \/
        PIPE, // |

        // Double Characters
        EQUAL, // ==
        NOT_EQUAL, // !=
        PLUS_EQ, // +=
        MINUS_EQ, // -=
        MULT_EQ, // *=
        DIV_EQ, // /=
        LT_EQUAL, // <=
        GT_EQUAL, // >=
        INCREMENT, // ++
        DECREMENT, // --
        COMMENT, // //
    }

    @Override
    public String toString() {
        return "Token {" + " literal: \"" + this.literal + "\" type: " + this.token_type + " }";
    }
}
