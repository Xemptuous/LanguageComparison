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

        this.readChar();
    }

    public Token nextToken() {
        while (Char.IsWhiteSpace(this.ch))
            this.readChar();

        Token tok;

        switch (this.ch) {
            case '\0': tok = new Token(TokenType.EOF, "\0"); break;
            case '*':  tok = new Token(TokenType.ASTERISK, "*"); break;
            case '/':  tok = new Token(TokenType.SLASH, "/"); break;
            case '-':  tok = new Token(TokenType.DASH, "-"); break;
            case '+':  tok = new Token(TokenType.PLUS, "+"); break;
            case '=':  tok = new Token(TokenType.EQUAL, "="); break;
            case '<':  tok = new Token(TokenType.LESSTHAN, "<"); break;
            case '>':  tok = new Token(TokenType.GREATERTHAN, ">"); break;
            case ';':  tok = new Token(TokenType.SEMICOLON, ";"); break;
            case ':':  tok = new Token(TokenType.COLON, ":"); break;
            case '(':  tok = new Token(TokenType.LPAREN, "("); break;
            case ')':  tok = new Token(TokenType.RPAREN, ")"); break;
            case '{':  tok = new Token(TokenType.LBRACE, "{"); break;
            case '}':  tok = new Token(TokenType.RBRACE, "}"); break;
            default:
                if (Char.IsLetter(this.ch)) {
                    string ident = this.readIdentifier();
                    TokenType tokenType =
                        Token.IDENTIFIER_MAP.GetValueOrDefault(ident, TokenType.IDENTIFIER);
                    return new Token(tokenType, ident);
                } else if (Char.IsDigit(this.ch))
                    return new Token(TokenType.NUMBER, this.readNumber());
                return new Token(TokenType.ILLEGAL, "ILLEGAL");
        }
        this.readChar();
        return tok;
    }

    private string readIdentifier() {
        int pos = this.curr;
        while (Char.IsAsciiLetter(this.ch) || this.ch == '_')
            this.readChar();
        return this.input.Substring(pos, this.curr - pos);
    }

    private string readNumber() {
        int pos = this.curr;
        while (Char.IsDigit(this.ch))
            this.readChar();
        return this.input.Substring(pos, this.curr - pos);
    }

    private void readChar() {
        this.ch   = this.peek >= this.input.Length ? '\0' : this.input.ElementAt(this.peek);
        this.curr = this.peek++;
    }
}
