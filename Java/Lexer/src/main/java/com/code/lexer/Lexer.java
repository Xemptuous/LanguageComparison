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

        this.readChar();
    }

    public Token nextToken() {
        Token tok = new Token(TokenType.EOF, "\0");

        while (Character.isWhitespace(this.ch))
            this.readChar();

        switch (this.ch) {
            case '\0':
                return tok;
            case '*':
                tok = new Token(TokenType.ASTERISK, "*");
                break;
            case '-':
                tok = new Token(TokenType.DASH, "-");
                break;
            case '+':
                tok = new Token(TokenType.PLUS, "+");
                break;
            case '=':
                tok = new Token(TokenType.EQUAL, "=");
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
            case ';':
                tok = new Token(TokenType.SEMICOLON, ";");
                break;
            case ':':
                tok = new Token(TokenType.COLON, ":");
                break;
            case '(':
                tok = new Token(TokenType.LPAREN, "(");
                break;
            case ')':
                tok = new Token(TokenType.RPAREN, ")");
                break;
            case '{':
                tok = new Token(TokenType.LBRACE, "{");
                break;
            case '}':
                tok = new Token(TokenType.RBRACE, "}");
                break;
            default:
                if (Character.isAlphabetic(this.ch)) {
                    String ident = this.readIdentifier();
                    TokenType ttype = Token.IDENTIFIER_MAP.get(ident);
                    return ttype != null ? new Token(ttype, ident) : new Token(TokenType.IDENTIFIER, ident);
                } else if (Character.isDigit(this.ch)) {
                    return new Token(TokenType.NUMBER, this.readNumber());
                } else {
                    return new Token(TokenType.ILLEGAL, "ILLEGAL");
                }
        }
        this.readChar();
        return tok;
    }

    private String readIdentifier() {
        int pos = this.curr;
        while (Character.isLetter(this.ch) || this.ch == '_')
            this.readChar();
        return this.input.substring(pos, this.curr);
    }

    private String readNumber() {
        int pos = this.curr;
        while (Character.isDigit(this.ch))
            this.readChar();
        return this.input.substring(pos, this.curr);
    }

    private void readChar() {
        this.ch = this.peek >= this.input.length() ? '\0' : this.input.charAt(this.peek);
        this.curr = this.peek++;
    }
}
