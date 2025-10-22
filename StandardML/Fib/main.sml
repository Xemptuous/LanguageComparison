fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)

fun print_fib_series n =
  let
    val nums = List.tabulate (Int.max (n + 1, 0), fn i => i)
    fun print_one x =
      let
        val f = fib x
      in
        print ("fib(" ^ Int.toString x ^ "): " ^ Int.toString f ^ "\n")
      end
  in
    List.app print_one nums
  end

val _ = print_fib_series 30
