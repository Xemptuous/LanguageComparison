open Printf

let rec fib n = match n with 0 -> 0 | 1 -> 1 | _ -> fib (n - 1) + fib (n - 2)

let main =
  for i = 0 to 30 do
    Printf.printf "fib(%d): %d\n" i (fib i)
  done
