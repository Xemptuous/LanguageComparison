object Fibonacci {
  def fib(n: Int): Int = {
    if (n <= 1) {
      n
    } else {
      fib(n - 1) + fib(n - 2)
    }
  }

  def main(args: Array[String]): Unit = {
    for (i <- 0 to 30) {
      println(s"fib(${i}): ${fib(i)}")
    }
  }
}
