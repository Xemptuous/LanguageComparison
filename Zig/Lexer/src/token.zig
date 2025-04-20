const std = @import("std");

const TokenType = enum {
    EOF,
    ILLEGAL,

    LET,
    FUNCTION,
    IF,
    ELSE,
    RETURN,

    IDENTIFIER,
    NUMBER,
    BOOLEAN,

    INT,
    BOOL,

    // Single Characters
    ASTERISK, // *
    SLASH, // /
    DASH, // -
    PLUS, // +
    EQUAL, // =
    LESSTHAN, // <
    GREATERTHAN, // >
    SEMICOLON, // ;
    COLON, // :
    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
};

pub const Token = struct {
    literal: []const u8,
    type: TokenType,

    pub fn new(ttype: TokenType, literal: []const u8) Token {
        return .{
            .type = ttype,
            .literal = literal,
        };
    }
};

pub const IdentifierMap = std.StaticStringMap(TokenType).initComptime(.{
    .{ "let", .LET },
    .{ "fn", .FUNCTION },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },

    .{ "true", .BOOLEAN },
    .{ "false", .BOOLEAN },

    .{ "bool", .BOOL },
    .{ "int", .INT },
});
