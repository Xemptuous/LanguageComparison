const std = @import("std");

pub const TokenType = enum {
    EOF,
    ILLEGAL,

    // Core
    LET,
    CONST,
    STRUCT,
    FUNCTION,
    IF,
    ELSE,
    SWITCH,
    CASE,
    BREAK,
    RETURN,
    WHILE,
    FOR,
    AND,
    OR,
    IN,

    // Types
    IDENTIFIER,
    NUMBER,
    FLOAT,
    BOOLEAN,
    STRING,
    CHAR,

    // Datatypes
    INT,
    F32,
    BOOL,
    STR,
    NIL,
    VOID,

    // Single Characters
    EXCLAMATION, // !
    AT, // @
    HASHTAG, // #
    DOLLAR, // $
    PERCENT, // %
    CARET, // ^
    AMPERSAND, // &
    ASTERISK, // *
    LPAREN, // (
    RPAREN, // )
    MINUS, // -
    UNDERSCORE, // _
    PLUS, // +
    ASSIGN, // =
    LBRACKET, // [
    RBRACKET, // ]
    LBRACE, // {
    RBRACE, // }
    SEMICOLON, // ;
    COLON, // :
    APOSTROPHE, // '
    QUOTE, // "
    COMMA, // ,
    PERIOD, // .
    LESSTHAN, // <
    GREATERTHAN, // >
    SLASH, // /
    QUESTION, // ?
    BACKSLASH, // /
    PIPE, // |

    // Double Characters
    EQUAL, // ==
    NOTEQUAL, // !=
    PLUSEQ, // +=
    MINUSEQ, // -=
    MULTEQ, // *=
    DIVEQ, // /=
    LTEQUAL, // <=
    GTEQUAL, // >=
    INCREMENT, // ++
    DECREMENT, // --
    COMMENT, // //

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
    .{ "const", .CONST },
    .{ "struct", .STRUCT },
    .{ "fn", .FUNCTION },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "switch", .SWITCH },
    .{ "case", .CASE },
    .{ "break", .BREAK },
    .{ "return", .RETURN },
    .{ "while", .WHILE },
    .{ "for", .FOR },
    .{ "and", .AND },
    .{ "or", .OR },
    .{ "in", .IN },

    .{ "true", .BOOLEAN },
    .{ "false", .BOOLEAN },

    .{ "bool", .BOOL },
    .{ "int", .INT },
    .{ "f32", .F32 },
    .{ "str", .STR },
    .{ "nil", .NIL },
    .{ "void", .VOID },
});

pub const DoubleTokenMap = std.StaticStringMap(TokenType).initComptime(.{
    .{ "==", .EQUAL },
    .{ "!=", .NOTEQUAL },
    .{ "+=", .PLUSEQ },
    .{ "-=", .MINUSEQ },
    .{ "*=", .MULTEQ },
    .{ "/=", .DIVEQ },
    .{ "<=", .LTEQUAL },
    .{ ">=", .GTEQUAL },
    .{ "++", .INCREMENT },
    .{ "--", .DECREMENT },
    .{ "//", .COMMENT },
});
