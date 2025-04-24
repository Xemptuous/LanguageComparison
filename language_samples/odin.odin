package main

import "core:fmt"

Point :: struct {
    x: int,
    y: int,
}

show :: proc(p: Point) {
    fmt.println("(", p.x, ",", p.y, ")")
}

greet :: proc(name: string) {
    fmt.println("Hi,", name, "!")
}

square :: proc(n: int) -> int {
    return n * n
}

main :: proc() {
    name := "Alice"
    x := 5
    y := 3.14
    active := true

    greet(name)
    fmt.println("Square of", x, "is", square(x))

    p := Point{3, 4}
    show(p)

    nums := []int{1, 2, 3}
    for n in nums {
        fmt.print(n, " ")
    }
    fmt.println()

    ages := map[string]int{"Alice": 30, "Bob": 25}
    fmt.println("Bob is", ages["Bob"], "years old.")

    count := 0
    for count < 3 {
        fmt.println("While loop:", count)
        count += 1
    }
}

