struct Point
    x::Int
    y::Int
end

function show(p::Point)
    println("(", p.x, ", ", p.y, ")")
end

greet(name) = println("Hi, ", name, "!")

square(n) = n * n

name = "Alice"
x = 5
y = 3.14
active = true

greet(name)
println("Square of ", x, " is ", square(x))

p = Point(3, 4)
show(p)

nums = [1, 2, 3]
for n in nums
    print(n, " ")
end
println()

ages = Dict("Alice" => 30, "Bob" => 25)
println("Bob is ", ages["Bob"], " years old.")

count = 0
while count < 3
    println("While loop: ", count)
    count += 1
end

