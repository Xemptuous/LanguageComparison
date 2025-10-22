// The Swift Programming Language
// https://docs.swift.org/swift-book

func fib(_ n: Int) -> Int {
    if n == 0 {
        return 0
    } else if n == 1 {
        return 1
    } else {
        return fib(n - 1) + fib(n - 2)
    }
}

@main
struct Fib {
    static func main() {
        for i in 0...30 {
            print("fib(\(i)): \(fib(i))")
        }
    }
}
