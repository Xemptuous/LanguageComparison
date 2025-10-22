fun fib(n: Int): Int {
    return when (n) {
        0 -> 0
        1 -> 1
        else -> fib(n - 1) + fib(n - 2)
    }
}

fun main() {
    for (i in 0..30) {
        println("fib($i): ${fib(i)}")
    }
}
