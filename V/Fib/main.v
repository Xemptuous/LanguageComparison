fn fib(n int) int {
    if n <= 1 {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

fn main() {
    for i := 0; i <= 30; i++ {
        println("fib($i): ${fib(i)}")
    }
}
