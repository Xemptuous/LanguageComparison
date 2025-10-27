fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)

val _ = app (fn i => 
  print ("fib(" ^ Int.toString i ^ "): " ^ Int.toString (fib i) ^ "\n")
) (List.tabulate(31, fn i => i))
