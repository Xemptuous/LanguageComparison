package main

import (
	"fmt"
)

type Point struct {
	x int
	y int
}

func (p Point) show() {
	fmt.Printf("(%d, %d)\n", p.x, p.y)
}

func greet(name string) {
	fmt.Println("Hi,", name+"!")
}

func square(n int) int {
	return n * n
}

func main() {
	name := "Alice"
	x := 5
	y := 3.14
	active := true

	greet(name)
	fmt.Printf("Square of %d is %d\n", x, square(x))

	p := Point{3, 4}
	p.show()

	nums := []int{1, 2, 3}
	for _, n := range nums {
		fmt.Print(n, " ")
	}
	fmt.Println()

	ages := map[string]int{"Alice": 30, "Bob": 25}
	fmt.Printf("Bob is %d years old.\n", ages["Bob"])

	count := 0
	for count < 3 {
		fmt.Printf("While loop: %d\n", count)
		count++
	}

	c := "green"
	if c == "green" {
		fmt.Println("Color is Green")
	}
}
