struct point {
  x: int,
  y: int
}

fun greet(name: string) {
  println("Hi, \(name)!")
}

fun square(n: int): int {
  n * n
}

val name = "Alice"
val x = 5
val y = 3.14
val active = true

greet(name)
println("Square of \(x) is \(square(x))")

val p = point { x = 3, y = 4 }
println("(\(p.x), \(p.y))")

val nums = [1, 2, 3]
for n in nums {
  print("\(n) ")
}
println("")

val ages = { "Alice": 30, "Bob": 25 }
println("Bob is \(ages["Bob"]) years old")

var count = 0
while (count < 3) {
  println("While loop: \(count)")
  count += 1
}

