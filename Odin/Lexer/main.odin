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

	input := `fn is_gt_ten(num: int) bool {
		if (num > 10) {
			return true;
		} else {
			return false;
		}
	}
	let ten = 5 + 5 * 4 / 2 - 5;
	print(is_big(ten));
	`


	l := lexer.newLexer(&input)

	tok := lexer.nextToken(&l)
	for tok.type != .EOF {
		fmt.println(tok)
		tok = lexer.nextToken(&l)
	}
}
