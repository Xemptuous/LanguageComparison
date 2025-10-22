def fib(n: int) -> int:
    if n == 0:
        return 0
    if n == 1:
        return 1
    return fib(n - 1) + fib(n - 2)


for i in range(0, 31):
    print(f"fib({i}): {fib(i)}")
