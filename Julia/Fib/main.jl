using Printf

function fib(n::Int)
    if n <= 1
        return n
    else
        return fib(n - 1) + fib(n - 2)
    end
end

function (@main)(args)
    for i in 0:30
        @printf("fib(%d): %d\n", i, fib(i))
    end
end
