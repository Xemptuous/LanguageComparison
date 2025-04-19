import std.stdio;
import std.ascii;

import token;

class Lexer
{
    uint curr;
    uint peek;
    char ch;
    string input;

    this(string input)
    {
        this.input = input;
        this.curr = 0;
        this.peek = 0;
        this.ch = '\0';

        this.readChar();
    }

    Token nextToken()
    {
        while (isWhite(this.ch))
            this.readChar();

        Token tok;

        switch (this.ch)
        {
        case '\0':
            return Token(TokenType.Eof, "\0");
        case '*':
            tok = Token(TokenType.Asterisk, "*");
            break;
        case '/':
            tok = Token(TokenType.Slash, "/");
            break;
        case '-':
            tok = Token(TokenType.Dash, "-");
            break;
        case '+':
            tok = Token(TokenType.Plus, "+");
            break;
        case '=':
            tok = Token(TokenType.Equal, "=");
            break;
        case '<':
            tok = Token(TokenType.Lessthan, "<");
            break;
        case '>':
            tok = Token(TokenType.Greaterthan, ">");
            break;
        case ';':
            tok = Token(TokenType.Semicolon, ";");
            break;
        case ':':
            tok = Token(TokenType.Colon, ":");
            break;
        case '(':
            tok = Token(TokenType.Lparen, "(");
            break;
        case ')':
            tok = Token(TokenType.Rparen, ")");
            break;
        case '{':
            tok = Token(TokenType.Lbrace, "{");
            break;
        case '}':
            tok = Token(TokenType.Rbrace, "}");
            break;
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
            string ident = this.readIdentifier();
            if (ident in IDENTIFIER_MAP)
                return Token(IDENTIFIER_MAP[ident], ident);
            else
                return Token(TokenType.Identifier, ident);
        case '0': .. case '9':
            return Token(TokenType.Number, this.readNumber());
        default:
            return Token(TokenType.Illegal, "ILLEGAL");
        }

        this.readChar();
        return tok;
    }

    string readIdentifier()
    {
        auto pos = this.curr;
        while (isAlpha(this.ch) || this.ch == '_')
            this.readChar();
        return this.input[pos .. this.curr];
    }

    string readNumber()
    {
        auto pos = this.curr;
        while (isDigit(this.ch))
            this.readChar();
        return this.input[pos .. this.curr];
    }

    void readChar()
    {
        if (this.peek >= this.input.length)
            this.ch = '\0';
        else
            this.ch = this.input[this.peek];
        this.curr = this.peek++;
    }
}
