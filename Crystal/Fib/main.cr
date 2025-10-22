def fib(n : Int) : Int
    if n <= 1
        return n
    else
        return fib(n-1) + fib(n-2)
    end
end

(0..30).each do |i|
    puts "fib(#{i}): #{fib(i)}"
end
