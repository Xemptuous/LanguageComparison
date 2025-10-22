package main

import (
	"fmt"
	"strconv"
)

func fib(n int) int {
	if n == 0 {
		return 0
	}
	if n == 1 {
		return 1
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	for i := range 31 {
		fmt.Println("fib(" + strconv.Itoa(i) + "): " + strconv.Itoa(fib(i)))
	}
}
