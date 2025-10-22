(defun fib (n)
  (cond
    ((<= n 1) n)
    (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun main () (loop for i from 0 to 30 do
                     (format t "fib(~d): ~d~%" i (fib i))))
