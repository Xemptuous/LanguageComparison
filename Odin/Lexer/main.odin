package main

import "core:fmt"
import "core:mem"

import "lexer"

MAX_SIZE: int : 50

main :: proc() {
	when ODIN_DEBUG {
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		context.allocator = mem.tracking_allocator(&track)

		defer {
			if len(track.allocation_map) > 0 {
				fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
				for _, entry in track.allocation_map {
					fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
				}
			}
			if len(track.bad_free_array) > 0 {
				fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
				for entry in track.bad_free_array {
					fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
				}
			}
			mem.tracking_allocator_destroy(&track)
		}
	}

	input := `
	struct Node {
		data: int;
		left: *Node;
		right: *Node;
	}
	fn add_nums(x: int, y: int) int {
		return x + y;
	}

	// check if str starts with an h
	fn starts_with_h(s: str) bool {
		return s[0] == 'H';
	}

	fn printNum(n: int|f32) void {
		print(n);
	}
	let rootNode = Node{5, nil, nil};
	print(rootNode.data);

	const ten: int = 5 + 5 * 4 / 2 - 5;
	const newNum = add_nums(ten, 25);
	const isTen = ten == 10 ? true : false;
	const nothing = nil;

	let mystring: str = "Hello, Lexer!";
	for ch: str in mystring {
		print(ch); // a comment here!
	}

	const myfloat: f32 = 69.420;

	let counter = 0; 
	let otherCounter = 20; 

	while counter < 10 or otherCounter > 10 {
		otherCounter -= 1;
		counter++;
	}
	`


	l := lexer.newLexer(&input)

	tok := lexer.nextToken(&l)
	for tok.type != .EOF {
		fmt.println(tok)
		tok = lexer.nextToken(&l)
	}
}
