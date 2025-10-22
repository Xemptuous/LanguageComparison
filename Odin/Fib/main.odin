package main

import "core:fmt"

fib :: proc(n: int) -> int {
	if n == 0 {
		return 0
	}
	if n == 1 {
		return 1
	}
	return fib(n - 1) + fib(n - 2)
}

main :: proc() {
	for i in 0 ..= 30 {
		fmt.printf("fib(%d): %d\n", i, fib(i))
	}
}
