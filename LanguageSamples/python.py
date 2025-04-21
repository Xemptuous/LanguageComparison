def greet(name):
    print(f"Hi, {name}!")


def square(n):
    return n * n


name = "Alice"
x = 5
y = 3.14
active = True

greet(name)
print(f"Square of {x} is {square(x)}")

point = {"x": 3, "y": 4}
print(f"({point['x']}, {point['y']})")

nums = [1, 2, 3]
for n in nums:
    print(n, end=" ")
print()

ages = {"Alice": 30, "Bob": 25}
print(f"Bob is {ages['Bob']} years old.")

count = 0
while count < 3:
    print(f"While loop: {count}")
    count += 1
