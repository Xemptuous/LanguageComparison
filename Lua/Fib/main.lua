local function fib(n)
    if n == 0 then
        return 0
    elseif n == 1 then
        return 1
    else
        return fib(n - 1) + fib(n - 2)
    end
end

for i = 0, 30 do
    print("fib(" .. i .. "): " .. fib(i))
end
