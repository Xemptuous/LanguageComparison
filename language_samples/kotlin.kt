data class Point(val x: Int, val y: Int)

fun greet(name: String) {
    println("Hi, $name!")
}

fun square(n: Int) = n * n

fun main() {
    val name = "Alice"
    val x = 5
    val y = 3.14
    val active = true

    greet(name)
    println("Square of $x is ${square(x)}")

    val p = Point(3, 4)
    println("(${p.x}, ${p.y})")

    val nums = listOf(1, 2, 3)
    for (n in nums) print("$n ")
    println()

    val ages = mapOf("Alice" to 30, "Bob" to 25)
    println("Bob is ${ages["Bob"]} years old.")

    var count = 0
    while (count < 3) {
        println("While loop: $count")
        count++
    }
}

