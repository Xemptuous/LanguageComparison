const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
    pub fn show(self: Point) void {
        std.debug.print("({}, {})\n", .{ self.x, self.y });
    }
};

fn greet(name: []const u8) void {
    std.debug.print("Hi, {}!\n", .{name});
}

fn square(n: i32) i32 {
    return n * n;
}

pub fn main() !void {
    const name = "Alice";
    const x = 5;
    const y = 3.14;
    const active = true;

    greet(name);
    std.debug.print("Square of {} is {}\n", .{ x, square(x) });

    const p = Point{ .x = 3, .y = 4 };
    p.show();

    const nums = [_]i32{ 1, 2, 3 };
    for (nums) |n| {
        std.debug.print("{} ", .{n});
    }
    std.debug.print("\n", .{});

    var ages = std.StringHashMap(i32).init(std.heap.page_allocator);
    try ages.put("Alice", 30);
    try ages.put("Bob", 25);
    const bob_age = ages.get("Bob") orelse 0;
    std.debug.print("Bob is {} years old.\n", .{bob_age});

    var count: i32 = 0;
    while (count < 3) : (count += 1) {
        std.debug.print("While loop: {}\n", .{count});
    }
}
