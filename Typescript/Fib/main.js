function fib(n) {
    if (n == 0)
        return 0;
    if (n == 1)
        return 1;
    return fib(n - 1) + fib(n - 2);
}
for (var i = 0; i <= 30; i++) {
    console.log("fib(".concat(i, "): ").concat(fib(i)));
}
