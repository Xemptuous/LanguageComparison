fun fib(n : int) : div int
  if n <= 0   then 0
  elif n == 1 then 1
  else fib(n - 1) + fib(n - 2)

fun main()
    for(0,30) fn(i)
        println("fib(" ++ i.show ++ "): " ++ fib(i).show)
