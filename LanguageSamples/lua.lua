function greet(name) print("Hi, " .. name .. "!") end

function square(n) return n * n end

name = "Alice"
x = 5
y = 3.14
active = true

greet(name)
print("Square of " .. x .. " is " .. square(x))

point = { x = 3, y = 4 }
print("(" .. point.x .. ", " .. point.y .. ")")

nums = { 1, 2, 3 }
for _, n in ipairs(nums) do
    io.write(n .. " ")
end
print()

ages = { Alice = 30, Bob = 25 }
print("Bob is " .. ages["Bob"] .. " years old.")

count = 0
while count < 3 do
    print("While loop: " .. count)
    count = count + 1
end
