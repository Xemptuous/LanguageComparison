var fib
fib = Fn.new {|n|
    if (n <= 1) {
        return n
    } else {
        return fib.call(n - 1) + fib.call(n - 2)
    }
}

for (i in 0..30) {
    System.print("fib(%(i)): %(fib.call(i))")
}
