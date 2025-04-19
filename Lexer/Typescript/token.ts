export enum TokenType {
  EOF = "EOF",
  ILLEGAL = "ILLEGAL",

  LET = "LET",
  FUNCTION = "FUNCTION",
  IF = "IF",
  ELSE = "ELSE",
  RETURN = "RETURN",

  IDENTIFIER = "IDENTIFIER",
  NUMBER = "NUMBER",
  BOOLEAN = "BOOLEAN",

  INT = "INT",
  BOOL = "BOOL",

  ASTERISK = "ASTERISK",
  SLASH = "SLASH",
  DASH = "DASH",
  PLUS = "PLUS",
  EQUAL = "EQUAL",
  LESSTHAN = "LESSTHAN",
  GREATERTHAN = "GREATERTHAN",
  SEMICOLON = "SEMICOLON",
  COLON = "COLON",
  LPAREN = "LPAREN",
  RPAREN = "RPAREN",
  LBRACE = "LBRACE",
  RBRACE = "RBRACE",
}

export type Token = {
  type: TokenType;
  literal: string;
};

export function newToken(type: TokenType, literal: string): Token {
  return { literal: literal, type: type };
}

export const IDENTIFIER_MAP = {
  "let": TokenType.LET,
  "fn": TokenType.FUNCTION,
  "if": TokenType.IF,
  "else": TokenType.ELSE,
  "return": TokenType.RETURN,

  "true": TokenType.BOOLEAN,
  "false": TokenType.BOOLEAN,

  "bool": TokenType.BOOL,
  "int": TokenType.INT,
}
