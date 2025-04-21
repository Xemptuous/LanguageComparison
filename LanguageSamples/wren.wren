class Point {
    construct new(x, y) {
        _x = x
        _y = y
    }

    show() {
        System.print("(%(_x), %(_y))")
    }
}

var greet = Fn.new { |name| System.print("Hi, %(name)!") }

var square = Fn.new { |n| n * n }

var name = "Alice"
var x = 5
var y = 3.14
var active = true

greet.call(name)
System.print("Square of %(x) is %(square.call(x))")

var p = Point.new(3, 4)
p.show()

var nums = [1, 2, 3]
for (n in nums) System.write("%(n) ")
System.print("")

var ages = { "Alice": 30, "Bob": 25 }
System.print("Bob is %(ages["Bob"]) years old.")

var count = 0
while (count < 3) {
    System.print("While loop: %(count)")
    count = count + 1
}

