const std = @import("std");
const Allocator = std.mem.Allocator;

fn make_jump_table(input: [*:0]const u8, gpa: Allocator) !std.AutoHashMap(usize, usize) {
    var map = std.AutoHashMap(usize, usize).init(gpa);
    var stack = try std.ArrayList(usize).initCapacity(gpa, 10);
    defer stack.deinit(gpa);
    const slice: []const u8 = std.mem.span(input);
    for (0.., slice) |i, ch| {
        switch (ch) {
            '[' => try stack.append(gpa, i),
            ']' => {
                const idx = stack.pop().?;
                try map.put(idx, i);
                try map.put(i, idx);
            },
            else => {},
        }
    }
    return map;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    var jump = try make_jump_table(input, allocator);
    defer jump.deinit();

    var tape = [_]u8{0} ** 3000;
    var dp: usize = 0;
    var ip: usize = 0;

    while (ip < input.len) {
        switch (input[ip]) {
            '>' => dp += 1,
            '<' => dp -= 1,
            '+' => tape[dp] += 1,
            '-' => tape[dp] -= 1,
            '.' => {
                std.debug.print("{c}", .{tape[dp]});
            },
            // ',' => {
            //     var buf: [1]u8 = undefined;
            //     var reader = std.fs.File.stdin().reader(&buf);
            //     _ = try reader.read(&buf);
            //     // std.debug.print("{c}", .{buf[0]});
            // },
            '[' => {
                if (tape[dp] == 0) {
                    ip = jump.get(ip).?;
                }
            },
            ']' => {
                if (tape[dp] != 0) {
                    ip = jump.get(ip).?;
                }
            },
            else => {},
        }
        ip += 1;
    }
}
