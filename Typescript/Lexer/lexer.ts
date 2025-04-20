import { Token, TokenType, newToken, IDENTIFIER_MAP } from "./token";

export class Lexer {
  private input: string;
  private ch: string;
  private curr: number;
  private peek: number;

  constructor (input: string) {
    this.input = input;
    this.ch = '\0';
    this.curr = 0;
    this.peek = 0;
    this.readChar();
  }

  public nextToken(): Token {
    let tok: Token = newToken(TokenType.EOF, "\0");

    while (isWhitespace(this.ch))
      this.readChar();

    switch (this.ch) {
      case '\0': return tok;
      case '*': tok = newToken(TokenType.ASTERISK, "*"); break;
      case '-': tok = newToken(TokenType.DASH, "-"); break;
      case '+': tok = newToken(TokenType.PLUS, "+"); break;
      case '=': tok = newToken(TokenType.EQUAL, "="); break;
      case '<': tok = newToken(TokenType.LESSTHAN, "<"); break;
      case '>': tok = newToken(TokenType.GREATERTHAN, ">"); break;
      case '/': tok = newToken(TokenType.SLASH, "/"); break;
      case ';': tok = newToken(TokenType.SEMICOLON, ";"); break;
      case ':': tok = newToken(TokenType.COLON, ";"); break;
      case '(': tok = newToken(TokenType.LPAREN, "("); break;
      case ')': tok = newToken(TokenType.RPAREN, ")"); break;
      case '{': tok = newToken(TokenType.LBRACE, "{"); break;
      case '}': tok = newToken(TokenType.RBRACE, "}"); break;
      default:
        if (isLetter(this.ch)) {
          const str = this.readIdentifier();
          const ttype = IDENTIFIER_MAP[str];
          return ttype == null || ttype == undefined ?
            newToken(TokenType.IDENTIFIER, str) :
            newToken(ttype, str);
        } else if (isDigit(this.ch))
          return newToken(TokenType.NUMBER, this.readNumber());
        else return newToken(TokenType.ILLEGAL, "ILLEGAL");
    }
    this.readChar();
    return tok;
  }

  private readIdentifier(): string {
    const pos = this.curr;
    while (isLetter(this.ch) || this.ch == '_')
      this.readChar();
    return this.input.substring(pos, this.curr);
  }

  private readNumber(): string {
    const pos = this.curr;
    while (isDigit(this.ch))
      this.readChar();
    return this.input.substring(pos, this.curr);
  }

  private readChar() {
    if (this.peek >= this.input.length)
      this.ch = '\0';
    else
      this.ch = this.input[this.peek];
    this.curr = this.peek++;
  }
}

function isLetter(str: string): boolean {
  return /^[a-xA-Z]+$/.test(str);
}

function isDigit(str: string): boolean {
  return /^[0-9]+$/.test(str);
}

function isWhitespace(str: string): boolean {
  return /^[\r\n\t\s]+$/.test(str);
}
