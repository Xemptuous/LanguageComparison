struct Point {
    var x: Int
    var y: Int
    func show() {
        print("(\(x), \(y))")
    }
}

enum Color {
    case red, green, blue
}

func greet(_ name: String) {
    print("Hi, \(name)!")
}

func square(_ n: Int) -> Int {
    return n * n
}

let name = "Alice"
let x = 5
let y = 3.14
let active = true

greet(name)
print("Square of \(x) is \(square(x))")

let p = Point(x: 3, y: 4)
p.show()

let nums = [1, 2, 3]
for n in nums {
    print(n, terminator: " ")
}
print()

let ages = ["Alice": 30, "Bob": 25]
if let bobAge = ages["Bob"] {
    print("Bob is \(bobAge) years old.")
}

var count = 0
while count < 3 {
    print("While loop: \(count)")
    count += 1
}

let c = Color.green
if c == .green {
    print("Color is Green")
}

