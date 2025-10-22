import Text.Printf (printf)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = do
  mapM_ (\i -> printf "fib(%d): %d\n" i (fib i)) [0 .. 30]
