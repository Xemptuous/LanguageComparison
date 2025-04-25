public class Lexer {
    private string input;
    private int curr;
    private int peek;
    private char ch;

    public Lexer(string input) {
        this.input = input;
        this.curr  = 0;
        this.peek  = 0;
        this.ch    = '\0';

        this.advance();
    }

    public Token nextToken() {
        while (Char.IsWhiteSpace(ch))
            advance();

        Token tok;

        if (peek < input.Length) {
            string ds = input[curr].ToString() + input[peek];
            if (ds == "//") return new Token(TokenType.COMMENT, readComment());
            if (Token.DOUBLE_TOKEN_MAP.ContainsKey(ds)) {
                advance();
                advance();
                return new Token(Token.DOUBLE_TOKEN_MAP[ds], ds);
            }
        }

        switch (ch) {
            case '\0': return new Token(TokenType.EOF, "\0");
            case '!':  tok = new Token(TokenType.EXCLAMATION, "!"); break;
            case '@':  tok = new Token(TokenType.AT, "@"); break;
            case '#':  tok = new Token(TokenType.HASHTAG, "#"); break;
            case '$':  tok = new Token(TokenType.DOLLAR, "$"); break;
            case '%':  tok = new Token(TokenType.PERCENT, "%"); break;
            case '^':  tok = new Token(TokenType.CARET, "^"); break;
            case '&':  tok = new Token(TokenType.AMPERSAND, "&"); break;
            case '*':  tok = new Token(TokenType.ASTERISK, "*"); break;
            case '(':  tok = new Token(TokenType.LPAREN, "("); break;
            case ')':  tok = new Token(TokenType.RPAREN, ")"); break;
            case '-':  tok = new Token(TokenType.MINUS, "-"); break;
            case '_':  tok = new Token(TokenType.UNDERSCORE, "_"); break;
            case '+':  tok = new Token(TokenType.PLUS, "+"); break;
            case '=':  tok = new Token(TokenType.ASSIGN, "="); break;
            case '[':  tok = new Token(TokenType.LBRACKET, "["); break;
            case ']':  tok = new Token(TokenType.RBRACKET, "]"); break;
            case '{':  tok = new Token(TokenType.LBRACE, "{"); break;
            case '}':  tok = new Token(TokenType.RBRACE, "}"); break;
            case ';':  tok = new Token(TokenType.SEMICOLON, ";"); break;
            case ':':  tok = new Token(TokenType.COLON, ":"); break;
            case '\'': tok = new Token(TokenType.CHAR, readChar()); break;
            case '"':  tok = new Token(TokenType.STRING, readString()); break;
            case ',':  tok = new Token(TokenType.COMMA, ","); break;
            case '.':  tok = new Token(TokenType.PERIOD, "."); break;
            case '<':  tok = new Token(TokenType.LESSTHAN, "<"); break;
            case '>':  tok = new Token(TokenType.GREATERTHAN, ">"); break;
            case '/':  tok = new Token(TokenType.SLASH, "/"); break;
            case '?':  tok = new Token(TokenType.QUESTION, "?"); break;
            case '\\': tok = new Token(TokenType.BACKSLASH, "\\"); break;
            case '|':  tok = new Token(TokenType.PIPE, "|"); break;
            default:
                if (Char.IsLetter(ch)) {
                    string ident = readIdentifier();
                    TokenType tokenType =
                        Token.IDENTIFIER_MAP.GetValueOrDefault(ident, TokenType.IDENTIFIER);
                    return new Token(tokenType, ident);
                }
                if (Char.IsDigit(ch)) {
                    Tuple<TokenType, string> res = readNumber();
                    return new Token(res.Item1, res.Item2);
                }
                tok = new Token(TokenType.ILLEGAL, "ILLEGAL");
                break;
        }
        advance();
        return tok;
    }

    private string readIdentifier() {
        int pos = curr;
        while (Char.IsAsciiLetterOrDigit(ch) || ch == '_')
            advance();
        return input.Substring(pos, curr - pos);
    }

    private Tuple<TokenType, string> readNumber() {
        int pos       = curr;
        bool is_float = false;
        while (Char.IsDigit(ch) || ch == '.') {
            if (ch == '.') {
                if (is_float) {
                    advance();
                    return new Tuple<TokenType, string>(TokenType.ILLEGAL, "ILLEGAL");
                }
                is_float = true;
            }
            advance();
        }
        return new Tuple<TokenType, string>(
            is_float ? TokenType.FLOAT : TokenType.NUMBER, input.Substring(pos, curr - pos)
        );
    }

    private string readString() {
        advance();
        int pos = curr;
        while (ch != 0 && ch != '\"')
            advance();
        return input.Substring(pos, curr - pos);
    }

    private string readChar() {
        advance();
        int pos = curr;
        while (ch != 0 && ch != '\'')
            advance();
        return input.Substring(pos, curr - pos);
    }

    private string readComment() {
        int pos = curr;
        while (ch != 0 && ch != '\n' && ch != '\r')
            advance();
        return input.Substring(pos, curr - pos);
    }

    private void advance() {
        ch   = peek >= input.Length ? '\0' : input.ElementAt(peek);
        curr = peek++;
    }
}
