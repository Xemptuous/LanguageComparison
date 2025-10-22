Red [Title: "Fib"]

fib: function [n] [
    if n < 2 [return n]
    (fib n - 1) + (fib n - 2)
]

i: 0
while [i <= 30 ] [
    print rejoin ["fib(" i "): " (fib i)]
    i: i + 1
]
