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
        this->readChar();

    Token tok = Token(::_EOF, "\0");
    switch (this->ch) {
        case '\0': tok = Token(::_EOF, "\0"); break;
        case '*':  tok = Token(::ASTERISK, "*"); break;
        case '/':  tok = Token(::SLASH, "/"); break;
        case '-':  tok = Token(::DASH, "-"); break;
        case '+':  tok = Token(::PLUS, "+"); break;
        case '=':  tok = Token(::EQUAL, "="); break;
        case '<':  tok = Token(::LESSTHAN, "<"); break;
        case '>':  tok = Token(::GREATERTHAN, ">"); break;
        case ';':  tok = Token(::SEMICOLON, ";"); break;
        case ':':  tok = Token(::COLON, ":"); break;
        case '(':  tok = Token(::LPAREN, "("); break;
        case ')':  tok = Token(::RPAREN, ")"); break;
        case '{':  tok = Token(::LBRACE, "{"); break;
        case '}':  tok = Token(::RBRACE, "}"); break;
        default:
            if (isalpha(this->ch)) {
                string ident = this->readIdentifier();
                if (IDENTIFIER_MAP.find(ident) == IDENTIFIER_MAP.end())
                    return Token(::IDENTIFIER, ident);
                else return Token(IDENTIFIER_MAP.at(ident), ident);
            } else if (isdigit(this->ch)) return Token(::NUMBER, this->readNumber());
            else tok = Token(::ILLEGAL, "ILLEGAL");
    };
    this->readChar();
    return tok;
}

string Lexer::readIdentifier() {
    uint64_t pos = this->curr;
    while (isalpha(this->ch) || this->ch == '_')
        this->readChar();
    return this->input.substr(pos, this->curr - pos);
}

string Lexer::readNumber() {
    uint64_t pos = this->curr;
    while (isdigit(this->ch))
        this->readChar();
    return this->input.substr(pos, this->curr - pos);
}

void Lexer::readChar() {
    this->ch   = this->peek >= this->input.length() ? 0 : this->input[this->peek];
    this->curr = this->peek++;
}
