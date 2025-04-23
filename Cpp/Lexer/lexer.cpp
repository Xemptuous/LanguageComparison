#include "lexer.hpp"

Lexer::Lexer(string input) {
    this->input = input;
    this->curr  = 0;
    this->peek  = 0;
    this->ch    = '\0';

    this->readChar();
}

Token Lexer::nextToken() {
    while (isspace(this->ch))
        readChar();

    Token tok;

    // if a peek is possible, try checking double tokens
    if (peek < input.length()) {
        string ds{input[curr], input[peek]};
        if (ds == "//") return Token(::COMMENT, readComment());
        if (DOUBLE_TOKEN_MAP.find(ds) != DOUBLE_TOKEN_MAP.end()) {
            readChar();
            readChar();
            return Token(DOUBLE_TOKEN_MAP.at(ds), ds);
        }
    }

    switch (this->ch) {
        case '\0': return Token(::_EOF, "\0"); break;
        case '!':  tok = Token(::EXCLAMATION, "!"); break;
        case '@':  tok = Token(::AT, "@"); break;
        case '#':  tok = Token(::HASHTAG, "#"); break;
        case '$':  tok = Token(::DOLLAR, "$"); break;
        case '%':  tok = Token(::PERCENT, "%"); break;
        case '^':  tok = Token(::CARET, "^"); break;
        case '&':  tok = Token(::AMPERSAND, "&"); break;
        case '*':  tok = Token(::ASTERISK, "*"); break;
        case '(':  tok = Token(::LPAREN, "("); break;
        case ')':  tok = Token(::RPAREN, ")"); break;
        case '-':  tok = Token(::MINUS, "-"); break;
        case '_':  tok = Token(::UNDERSCORE, "_"); break;
        case '+':  tok = Token(::PLUS, "+"); break;
        case '=':  tok = Token(::ASSIGN, "="); break;
        case '[':  tok = Token(::LBRACKET, "["); break;
        case ']':  tok = Token(::RBRACKET, "]"); break;
        case '{':  tok = Token(::LBRACE, "{"); break;
        case '}':  tok = Token(::RBRACE, "}"); break;
        case ';':  tok = Token(::SEMICOLON, ";"); break;
        case ':':  tok = Token(::COLON, ":"); break;
        case '\'': tok = Token(::CHAR, readCharLiteral()); break;
        case '"':  tok = Token(::STRING, readString()); break;
        case ',':  tok = Token(::COMMA, ","); break;
        case '.':  tok = Token(::PERIOD, "."); break;
        case '<':  tok = Token(::LESSTHAN, "<"); break;
        case '>':  tok = Token(::GREATERTHAN, ">"); break;
        case '/':  tok = Token(::SLASH, "/"); break;
        case '?':  tok = Token(::QUESTION, "?"); break;
        case '\\': tok = Token(::BACKSLASH, "\\"); break;
        case '|':  tok = Token(::PIPE, "|"); break;
        default:
            // identifier
            if (isalpha(ch)) {
                string ident = readIdentifier();
                if (IDENTIFIER_MAP.find(ident) == IDENTIFIER_MAP.end())
                    return Token(::IDENTIFIER, ident);
                return Token(IDENTIFIER_MAP.at(ident), ident);
            }
            // number
            if (isdigit(ch)) {
                pair<TokenType, string> res = readNumber();
                return Token(res.first, res.second);
            }
            tok = Token(::ILLEGAL, "ILLEGAL");
    };
    readChar();
    return tok;
}

string Lexer::readIdentifier() {
    uint64_t pos = curr;
    while (isalpha(ch) || isdigit(ch) || ch == '_')
        readChar();
    return input.substr(pos, curr - pos);
}

pair<TokenType, string> Lexer::readNumber() {
    uint64_t pos  = curr;
    bool is_float = false;
    while (isdigit(ch) || ch == '.') {
        if (ch == '.') {
            if (is_float) {
                readChar();
                return {::ILLEGAL, "ILLEGAL"};
            }
            is_float = true;
        }
        readChar();
    }
    return {is_float ? ::FLOAT : ::NUMBER, input.substr(pos, curr - pos)};
}

string Lexer::readString() {
    readChar(); // skip "
    uint64_t pos = curr;
    while (ch != '\"' && ch != 0)
        readChar();
    return input.substr(pos, curr - pos);
}

string Lexer::readCharLiteral() {
    readChar(); // skip '
    uint64_t pos = curr;
    while (ch != '\'' && ch != 0)
        readChar();
    return input.substr(pos, curr - pos);
}

string Lexer::readComment() {
    uint64_t pos = curr;
    while (ch != 0 && ch != '\n' && ch != '\r')
        readChar();
    return input.substr(pos, curr - pos);
}

void Lexer::readChar() {
    ch   = peek >= input.length() ? 0 : input[peek];
    curr = peek++;
}

char Lexer::peekChar() {
    return peek >= input.length() ? 0 : input[peek];
}
