type
  Point = object
    x, y: int

proc show(p: Point) =
  echo "(", p.x, ", ", p.y, ")"

proc greet(name: string) =
  echo "Hi, ", name, "!"

proc square(n: int): int = n * n

var name = "Alice"
var x = 5
var y = 3.14
var active = true

greet(name)
echo "Square of ", x, " is ", square(x)

var p = Point(x: 3, y: 4)
p.show()

let nums = @[1, 2, 3]
for n in nums:
  stdout.write($n & " ")
echo ""

let ages = {"Alice": 30, "Bob": 25}.toTable
echo "Bob is ", ages["Bob"], " years old."

var count = 0
while count < 3:
  echo "While loop: ", count
  inc count

