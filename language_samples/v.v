struct Point {
    x int
    y int
}

fn greet(name string) {
    println('Hi, $name!')
}

fn square(n int) int {
    return n * n
}

fn main() {
    name := 'Alice'
    x := 5
    y := 3.14
    active := true

    greet(name)
    println('Square of $x is ${square(x)}')

    p := Point{3, 4}
    println('($p.x, $p.y)')

    nums := [1, 2, 3]
    for n in nums {
        print('$n ')
    }
    println('')

    ages := {
        'Alice': 30
        'Bob': 25
    }
    println('Bob is $ages["Bob"] years old.')

    mut count := 0
    for count < 3 {
        println('While loop: $count')
        count++
    }
}

