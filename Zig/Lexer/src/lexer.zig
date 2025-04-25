const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const IdentifierMap = token.IdentifierMap;
const DoubleTokenMap = token.DoubleTokenMap;

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

        if (self.peek < self.input.len) {
            const ds = self.input[self.curr .. self.peek + 1];
            if (std.mem.eql(u8, ds, "//"))
                return Token.new(.COMMENT, self.readComment());
            const ttype = DoubleTokenMap.get(ds);
            if (ttype != null) {
                self.readChar();
                self.readChar();
                return Token.new(ttype.?, ds);
            }
        }

        const tok = switch (self.char.?) {
            0 => Token.new(.EOF, "\\0"),
            '!' => Token.new(.EXCLAMATION, "!"),
            '@' => Token.new(.AT, "@"),
            '#' => Token.new(.HASHTAG, "#"),
            '$' => Token.new(.DOLLAR, "$"),
            '%' => Token.new(.PERCENT, "%"),
            '^' => Token.new(.CARET, "^"),
            '&' => Token.new(.AMPERSAND, "&"),
            '*' => Token.new(.ASTERISK, "*"),
            '(' => Token.new(.LPAREN, "("),
            ')' => Token.new(.RPAREN, ")"),
            '-' => Token.new(.MINUS, "-"),
            '_' => Token.new(.UNDERSCORE, "_"),
            '+' => Token.new(.PLUS, "+"),
            '=' => Token.new(.ASSIGN, "="),
            '[' => Token.new(.LBRACKET, "["),
            ']' => Token.new(.RBRACKET, "]"),
            '{' => Token.new(.LBRACE, "{"),
            '}' => Token.new(.RBRACE, "}"),
            ';' => Token.new(.SEMICOLON, ";"),
            ':' => Token.new(.COLON, ":"),
            '\'' => Token.new(.CHAR, self.readCharLiteral()),
            '"' => Token.new(.STRING, self.readString()),
            ',' => Token.new(.COMMA, ","),
            '.' => Token.new(.PERIOD, "."),
            '<' => Token.new(.LESSTHAN, "<"),
            '>' => Token.new(.GREATERTHAN, ">"),
            '/' => Token.new(.SLASH, "/"),
            '?' => Token.new(.QUESTION, "?"),
            '\\' => Token.new(.BACKSLASH, "\\"),
            '|' => Token.new(.PIPE, "|"),
            '0'...'9' => {
                const res = self.readNumber();
                return Token.new(res.ttype, res.literal);
            },
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

    pub fn readString(self: *Lexer) []const u8 {
        self.readChar();
        const pos = self.curr;
        while (self.char != '"')
            self.readChar();
        return self.input[pos..self.curr];
    }

    pub fn readCharLiteral(self: *Lexer) []const u8 {
        self.readChar();
        const pos = self.curr;
        while (self.char != '\'')
            self.readChar();
        return self.input[pos..self.curr];
    }

    pub fn readComment(self: *Lexer) []const u8 {
        const pos = self.curr;
        while (self.char != '\n' and self.char != '\r')
            self.readChar();
        return self.input[pos..self.curr];
    }

    pub fn readNumber(self: *Lexer) struct { ttype: token.TokenType, literal: []const u8 } {
        const pos = self.curr;
        var is_float = false;
        while (std.ascii.isDigit(self.char.?) or self.char == '.') {
            if (self.char == '.') {
                if (is_float) {
                    self.readChar();
                    return .{ .ttype = .ILLEGAL, .literal = "ILLEGAL" };
                }
                is_float = true;
            }
            self.readChar();
        }
        if (is_float)
            return .{ .ttype = .FLOAT, .literal = self.input[pos..self.curr] };
        return .{ .ttype = .NUMBER, .literal = self.input[pos..self.curr] };
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
