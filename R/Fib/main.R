fib <- function(n) {
    if (n <= 1) return(1)
    return(fib(n - 1) + fib(n - 2))
}

for (i in 0:30)
    print(paste("fib(", i, "): ", fib(i)))
