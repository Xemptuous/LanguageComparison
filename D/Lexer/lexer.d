import std.stdio;
import std.ascii;

import token;
import std.typecons : tuple, Tuple;
import std.conv;

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

        if (this.peek < this.input.length)
        {
            string ds = to!string(this.input[this.curr]) ~ to!string(this.input[this.peek]);
            if (ds == "//")
            {
                return Token(TokenType.Comment, this.readComment());
            }
            if (ds in DOUBLE_TOKEN_MAP)
            {
                this.readChar();
                this.readChar();
                return Token(DOUBLE_TOKEN_MAP[ds], ds);
            }
        }

        switch (this.ch)
        {
        case '\0':
            return Token(TokenType.Eof, "\0");
        case '!':
            tok = Token(TokenType.Exclamation, "!");
            break;
        case '@':
            tok = Token(TokenType.At, "@");
            break;
        case '#':
            tok = Token(TokenType.Hashtag, "#");
            break;
        case '$':
            tok = Token(TokenType.Dollar, "$");
            break;
        case '%':
            tok = Token(TokenType.Percent, "%");
            break;
        case '^':
            tok = Token(TokenType.Caret, "^");
            break;
        case '&':
            tok = Token(TokenType.Ampersand, "&");
            break;
        case '*':
            tok = Token(TokenType.Asterisk, "*");
            break;
        case '(':
            tok = Token(TokenType.Lparen, "(");
            break;
        case ')':
            tok = Token(TokenType.Rparen, ")");
            break;
        case '-':
            tok = Token(TokenType.Minus, "-");
            break;
        case '_':
            tok = Token(TokenType.Underscore, "_");
            break;
        case '+':
            tok = Token(TokenType.Plus, "+");
            break;
        case '=':
            tok = Token(TokenType.Assign, "=");
            break;
        case '[':
            tok = Token(TokenType.Lbracket, "[");
            break;
        case ']':
            tok = Token(TokenType.Rbracket, "]");
            break;
        case '{':
            tok = Token(TokenType.Lbrace, "{");
            break;
        case '}':
            tok = Token(TokenType.Rbrace, "}");
            break;
        case ';':
            tok = Token(TokenType.Semicolon, ";");
            break;
        case ':':
            tok = Token(TokenType.Colon, ":");
            break;
        case '\'':
            tok = Token(TokenType.Char, "'");
            break;
        case '"':
            tok = Token(TokenType.String, "\"");
            break;
        case ',':
            tok = Token(TokenType.Comma, ",");
            break;
        case '.':
            tok = Token(TokenType.Period, ".");
            break;
        case '<':
            tok = Token(TokenType.Lessthan, "<");
            break;
        case '>':
            tok = Token(TokenType.Greaterthan, ">");
            break;
        case '/':
            tok = Token(TokenType.Slash, "/");
            break;
        case '?':
            tok = Token(TokenType.Question, "?");
            break;
        case '\\':
            tok = Token(TokenType.Backslash, "\\");
            break;
        case '|':
            tok = Token(TokenType.Pipe, "|");
            break;
        case 'a': .. case 'z':
        case 'A': .. case 'Z':
            string ident = this.readIdentifier();
            if (ident in IDENTIFIER_MAP)
                return Token(IDENTIFIER_MAP[ident], ident);
            else
                return Token(TokenType.Identifier, ident);
        case '0': .. case '9':
            auto res = this.readNumber();
            return Token(res[0], res[1]);
        default:
            tok = Token(TokenType.Illegal, "ILLEGAL");
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

    Tuple!(TokenType, string) readNumber()
    {
        auto pos = this.curr;
        bool is_float = false;
        while (isDigit(this.ch) || this.ch == '.')
        {
            if (this.ch == '.')
            {
                if (is_float)
                {
                    return tuple(TokenType.Illegal, "ILLEGAL");
                }
                is_float = true;
            }
            this.readChar();
        }
        if (is_float)
        {
            return tuple(TokenType.Float, this.input[pos .. this.curr]);
        }
        return tuple(TokenType.Number, this.input[pos .. this.curr]);
    }

    string readString()
    {
        auto pos = this.curr;
        while (this.ch != '\0' && this.ch != '"')
            this.readChar();
        return this.input[pos .. this.curr];
    }

    string readCharLiteral()
    {
        auto pos = this.curr;
        while (this.ch != '\0' && this.ch != '\'')
            this.readChar();
        return this.input[pos .. this.curr];
    }

    string readComment()
    {
        auto pos = this.curr + 2;
        while (this.ch != '\0' && this.ch != '\n' && this.ch != '\r')
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
