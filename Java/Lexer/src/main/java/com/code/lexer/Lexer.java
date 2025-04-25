package com.code.lexer;

import com.code.lexer.Token.TokenType;

public class Lexer {
    private String input;
    private int curr;
    private int peek;
    private char ch;

    Lexer(String input) {
        this.input = input;
        this.curr = 0;
        this.peek = 0;
        this.ch = '\0';

        this.advance();
    }

    private static class Pair {
        TokenType type;
        String literal;

        Pair(TokenType t, String l) {
            this.type = t;
            this.literal = l;
        }
    }

    public Token nextToken() {
        Token tok = new Token(TokenType.EOF, "\0");

        while (Character.isWhitespace(ch))
            advance();

        if (peek < input.length()) {
            String ds = String.valueOf(input.charAt(curr)) + input.charAt(peek);
            if (ds.charAt(0) == '/' && ds.charAt(1) == '/') {
                return new Token(TokenType.COMMENT, readComment());
            } else if (Token.DOUBLE_TOKEN_MAP.containsKey(ds)) {
                advance();
                advance();
                return new Token(Token.DOUBLE_TOKEN_MAP.get(ds), ds);
            }
        }

        switch (ch) {
            case '\0':
                return tok;
            case '!':
                tok = new Token(TokenType.EXCLAMATION, "!");
                break;
            case '@':
                tok = new Token(TokenType.AT, "@");
                break;
            case '#':
                tok = new Token(TokenType.HASHTAG, "#");
                break;
            case '$':
                tok = new Token(TokenType.DOLLAR, "$");
                break;
            case '%':
                tok = new Token(TokenType.PERCENT, "%");
                break;
            case '^':
                tok = new Token(TokenType.CARET, "^");
                break;
            case '&':
                tok = new Token(TokenType.AMPERSAND, "&");
                break;
            case '*':
                tok = new Token(TokenType.ASTERISK, "*");
                break;
            case '(':
                tok = new Token(TokenType.LPAREN, "(");
                break;
            case ')':
                tok = new Token(TokenType.RPAREN, ")");
                break;
            case '-':
                tok = new Token(TokenType.MINUS, "-");
                break;
            case '_':
                tok = new Token(TokenType.UNDERSCORE, "_");
                break;
            case '+':
                tok = new Token(TokenType.PLUS, "+");
                break;
            case '=':
                tok = new Token(TokenType.ASSIGN, "=");
                break;
            case '[':
                tok = new Token(TokenType.LBRACKET, "[");
                break;
            case ']':
                tok = new Token(TokenType.RBRACKET, "]");
                break;
            case '{':
                tok = new Token(TokenType.LBRACE, "{");
                break;
            case '}':
                tok = new Token(TokenType.RBRACE, "}");
                break;
            case ';':
                tok = new Token(TokenType.SEMICOLON, ";");
                break;
            case ':':
                tok = new Token(TokenType.COLON, ":");
                break;
            case '\'':
                tok = new Token(TokenType.CHAR, readChar());
                break;
            case '"':
                tok = new Token(TokenType.STRING, readString());
                break;
            case ',':
                tok = new Token(TokenType.COMMA, ",");
                break;
            case '.':
                tok = new Token(TokenType.PERIOD, ".");
                break;
            case '<':
                tok = new Token(TokenType.LESSTHAN, "<");
                break;
            case '>':
                tok = new Token(TokenType.GREATERTHAN, ">");
                break;
            case '/':
                tok = new Token(TokenType.SLASH, "/");
                break;
            case '?':
                tok = new Token(TokenType.QUESTION, "?");
                break;
            case '\\':
                tok = new Token(TokenType.BACKSLASH, "\\");
                break;
            case '|':
                tok = new Token(TokenType.PIPE, "|");
                break;
            default:
                if (Character.isAlphabetic(ch)) {
                    String ident = readIdentifier();
                    TokenType ttype = Token.IDENTIFIER_MAP.getOrDefault(ident, TokenType.IDENTIFIER);
                    return new Token(ttype, ident);
                } else if (Character.isDigit(ch)) {
                    Pair num = readNumber();
                    return new Token(num.type, num.literal);
                } else {
                    return new Token(TokenType.ILLEGAL, "ILLEGAL");
                }
        }
        advance();
        return tok;
    }

    private String readIdentifier() {
        int pos = curr;
        while (Character.isLetterOrDigit(ch) || ch == '_')
            advance();
        return input.substring(pos, curr);
    }

    private Pair readNumber() {
        int pos = curr;
        boolean is_float = false;
        while (Character.isDigit(ch) || ch == '.') {
            if (ch == '.') {
                if (is_float) {
                    advance();
                    return new Pair(TokenType.ILLEGAL, "ILLEGAL");
                }
                is_float = true;
            }
            advance();
        }

        return new Pair(is_float ? TokenType.FLOAT : TokenType.NUMBER, input.substring(pos, curr));
    }

    private String readString() {
        advance();
        int pos = curr;
        while (ch != '\0' && ch != '\"')
            advance();
        return input.substring(pos, curr);
    }

    private String readChar() {
        advance();
        int pos = curr;
        while (ch != '\0' && ch != '\'')
            advance();
        return input.substring(pos, curr);
    }

    private String readComment() {
        int pos = curr;
        while (ch != '\0' && ch != '\n' && ch != '\r')
            advance();
        return input.substring(pos, curr);
    }

    private void advance() {
        ch = peek >= input.length() ? '\0' : input.charAt(peek);
        curr = peek++;
    }
}
