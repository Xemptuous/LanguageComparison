const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const IdentifierMap = token.IdentifierMap;

pub const Lexer = struct {
    input: []const u8,
    curr: usize,
    peek: usize,
    char: ?u8,

    pub fn new(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
            .curr = 0,
            .peek = 0,
            .char = '0',
        };
        lexer.readChar();
        return lexer;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        while (std.ascii.isWhitespace(self.char.?))
            self.readChar();

        const tok = switch (self.char.?) {
            0 => Token.new(.EOF, "\\0"),
            '*' => Token.new(.ASTERISK, "*"),
            '/' => Token.new(.SLASH, "/"),
            '-' => Token.new(.DASH, "-"),
            '+' => Token.new(.PLUS, "+"),
            '=' => Token.new(.EQUAL, "="),
            '<' => Token.new(.LESSTHAN, "<"),
            '>' => Token.new(.GREATERTHAN, ">"),
            ';' => Token.new(.SEMICOLON, ";"),
            ':' => Token.new(.COLON, ":"),
            '(' => Token.new(.LPAREN, "("),
            ')' => Token.new(.RPAREN, ")"),
            '{' => Token.new(.LBRACE, "{"),
            '}' => Token.new(.RBRACE, "}"),
            '0'...'9' => return Token.new(.NUMBER, self.readNumber()),
            'a'...'z', 'A'...'Z' => {
                const ident = self.readIdentifier();
                const lookup = IdentifierMap.get(ident);

                if (lookup == null)
                    return Token.new(.IDENTIFIER, ident)
                else
                    return Token.new(lookup.?, ident);
            },
            else => Token.new(.ILLEGAL, "ILLEGAL"),
        };
        self.readChar();
        return tok;
    }

    pub fn readIdentifier(self: *Lexer) []const u8 {
        const pos = self.curr;
        while (std.ascii.isAlphanumeric(self.char.?) or self.char.? == '_')
            self.readChar();
        return self.input[pos..self.curr];
    }

    pub fn readNumber(self: *Lexer) []const u8 {
        const pos = self.curr;
        while (std.ascii.isDigit(self.char.?))
            self.readChar();
        return self.input[pos..self.curr];
    }

    pub fn readChar(self: *Lexer) void {
        if (self.peek >= self.input.len)
            self.char = 0
        else
            self.char = self.input[self.peek];

        self.curr = self.peek;
        self.peek += 1;
    }
};
