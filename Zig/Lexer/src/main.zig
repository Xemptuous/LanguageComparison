const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const token = @import("token.zig");

pub fn main() !void {
    const input =
        \\ struct Node {
        \\     data: int;
        \\     left: *Node;
        \\     right: *Node;
        \\ }
        \\ fn add_nums(x: int, y: int) int {
        \\     return x + y;
        \\ }
        \\ 
        \\ // check if str starts with an h
        \\ fn starts_with_h(s: str) bool {
        \\     return s[0] == 'H';
        \\ }
        \\ 
        \\ fn printNum(n: int|f32) void {
        \\     print(n);
        \\ }
        \\ let rootNode = Node{5, nil, nil};
        \\ print(rootNode.data);
        \\ 
        \\ const ten: int = 5 + 5 * 4 / 2 - 5;
        \\ const newNum = add_nums(ten, 25);
        \\ const isTen = ten == 10 ? true : false;
        \\ const nothing = nil;
        \\ 
        \\ let mystring: str = "Hello, Lexer!";
        \\ for ch: str in mystring {
        \\     print(ch); // a comment here!
        \\ }
        \\ 
        \\ const myfloat: f32 = 69.420;
        \\ 
        \\ let counter = 0; 
        \\ let otherCounter = 20; 
        \\ 
        \\ while counter < 10 or otherCounter > 10 {
        \\     otherCounter -= 1;
        \\     counter++;
        \\ }
    ;
    var lexer = Lexer.new(input);

    var tok = lexer.nextToken();
    while (tok.type != .EOF) {
        std.debug.print("Token: literal: \"{s}\" type: {}\n", .{ tok.literal, tok.type });
        tok = lexer.nextToken();
    }
}
