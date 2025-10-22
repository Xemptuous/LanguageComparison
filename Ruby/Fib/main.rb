def fib(n)
  return n if n < 2
  fib(n - 1) + fib(n - 2)
end

for i in 0..30 do
  puts "fib(#{i}): #{fib(i)}"
end
