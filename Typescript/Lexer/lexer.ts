import { Token, TokenType, newToken, IDENTIFIER_MAP, DOUBLE_TOKEN_MAP } from "./token";

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
    this.advance();
  }

  public nextToken(): Token {
    let tok: Token = newToken(TokenType.EOF, "\0");

    while (isWhitespace(this.ch))
      this.advance();

    if (this.peek < this.input.length) {
      const ds: string = this.input[this.curr] + this.input[this.peek];
      if (ds == "//")
        return newToken(TokenType.COMMENT, this.readComment());
      const ttype = DOUBLE_TOKEN_MAP[ds];
      if (ttype != null && ttype != undefined) {
        this.advance();
        this.advance();
        return newToken(ttype, ds);
      }
    }

    switch (this.ch) {
      case '\0': return tok;
      case '!': tok = newToken(TokenType.EXCLAMATION, "!"); break;
      case '@': tok = newToken(TokenType.AT, "@"); break;
      case '#': tok = newToken(TokenType.HASHTAG, "#"); break;
      case '$': tok = newToken(TokenType.DOLLAR, "$"); break;
      case '%': tok = newToken(TokenType.PERCENT, "%"); break;
      case '^': tok = newToken(TokenType.CARET, "^"); break;
      case '&': tok = newToken(TokenType.AMPERSAND, "&"); break;
      case '*': tok = newToken(TokenType.ASTERISK, "*"); break;
      case '(': tok = newToken(TokenType.LPAREN, "("); break;
      case ')': tok = newToken(TokenType.RPAREN, ")"); break;
      case '-': tok = newToken(TokenType.MINUS, "-"); break;
      case '_': tok = newToken(TokenType.UNDERSCORE, "_"); break;
      case '+': tok = newToken(TokenType.PLUS, "+"); break;
      case '=': tok = newToken(TokenType.ASSIGN, "="); break;
      case '[': tok = newToken(TokenType.LBRACKET, "["); break;
      case ']': tok = newToken(TokenType.RBRACKET, "]"); break;
      case '{': tok = newToken(TokenType.LBRACE, "{"); break;
      case '}': tok = newToken(TokenType.RBRACE, "}"); break;
      case ';': tok = newToken(TokenType.SEMICOLON, ";"); break;
      case ':': tok = newToken(TokenType.COLON, ":"); break;
      case '\'': tok = newToken(TokenType.CHAR, this.readChar()); break;
      case '"': tok = newToken(TokenType.STRING, this.readString()); break;
      case ',': tok = newToken(TokenType.COMMA, ","); break;
      case '.': tok = newToken(TokenType.PERIOD, "."); break;
      case '<': tok = newToken(TokenType.LESSTHAN, "<"); break;
      case '>': tok = newToken(TokenType.GREATERTHAN, ">"); break;
      case '/': tok = newToken(TokenType.SLASH, "/"); break;
      case '?': tok = newToken(TokenType.QUESTION, "?"); break;
      case '\\': tok = newToken(TokenType.BACKSLASH, "\\"); break;
      case '|': tok = newToken(TokenType.PIPE, "|"); break;
      default:
        if (isLetter(this.ch)) {
          const str = this.readIdentifier();
          const ttype = IDENTIFIER_MAP[str];
          return ttype == null || ttype == undefined ?
            newToken(TokenType.IDENTIFIER, str) :
            newToken(ttype, str);
        } else if (isDigit(this.ch)) {
          const [ttype, lit] = this.readNumber();
          return newToken(ttype, lit);
        }
        return newToken(TokenType.ILLEGAL, "ILLEGAL");
    }
    this.advance();
    return tok;
  }

  private readIdentifier(): string {
    const pos = this.curr;
    while (isAlphanumeric(this.ch) || this.ch == '_')
      this.advance();
    return this.input.substring(pos, this.curr);
  }

  private readString(): string {
    this.advance();
    const pos = this.curr;
    while (this.ch != '"' && this.ch != '\0')
      this.advance();
    return this.input.substring(pos, this.curr);
  }

  private readChar(): string {
    this.advance();
    const pos = this.curr;
    while (this.ch != "'" && this.ch != '\0')
      this.advance();
    return this.input.substring(pos, this.curr);
  }

  private readComment(): string {
    const pos = this.curr;
    while (this.ch != '\n' && this.ch != '\r' && this.ch != '\0')
      this.advance();
    return this.input.substring(pos, this.curr);
  }

  private readNumber(): [TokenType, string] {
    const pos = this.curr;
    let is_float = false;
    while (isDigit(this.ch) || this.ch == '.') {
      if (this.ch == '.') {
        if (is_float) {
          this.advance();
          return [TokenType.ILLEGAL, "ILLEGAL"];
        }
        is_float = true;
      }
      this.advance();
    }
    if (is_float)
      return [TokenType.FLOAT, this.input.substring(pos, this.curr)];
    return [TokenType.NUMBER, this.input.substring(pos, this.curr)];
  }

  private advance() {
    if (this.peek >= this.input.length)
      this.ch = '\0';
    else
      this.ch = this.input[this.peek];
    this.curr = this.peek++;
  }
}

function isAlphanumeric(str: string): boolean {
  return /^[a-zA-Z0-9]+$/.test(str);
}

function isLetter(str: string): boolean {
  return /^[a-zA-Z]+$/.test(str);
}

function isDigit(str: string): boolean {
  return /^[0-9]+$/.test(str);
}

function isWhitespace(str: string): boolean {
  return /^[\r\n\t\s]+$/.test(str);
}
