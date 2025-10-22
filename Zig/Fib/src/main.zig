const std = @import("std");

fn fib(n: i32) i32 {
    if (n == 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    }
    return fib(n - 1) + fib(n - 2);
}

pub fn main() !void {
    var i: i32 = 0;
    while (i <= 30) : (i += 1) {
        std.debug.print("fib({}): {}\n", .{ i, fib(i) });
    }
}
