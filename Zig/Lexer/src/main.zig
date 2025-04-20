const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const token = @import("token.zig");

pub fn main() !void {
    const input =
        \\ fn is_gt_ten(num: int) bool {
        \\        if (num > 10) {
        \\            return true;
        \\        } else {
        \\            return false;
        \\        }
        \\    }
        \\    let ten = 5 + 5 * 4 / 2 - 5;
        \\    print(is_big(ten));
    ;
    var lexer = Lexer.new(input);

    var tok = lexer.nextToken();
    while (tok.type != .EOF) {
        std.debug.print("Token: literal: \"{s}\" type: {}\n", .{ tok.literal, tok.type });
        tok = lexer.nextToken();
    }
}
