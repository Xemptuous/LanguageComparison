(fn fib [n]
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(for [i 0 30]
  (print (string.format "fib(%d): %d" i (fib i))))
