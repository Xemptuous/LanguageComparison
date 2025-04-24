(defstruct point x y)

(defun greet (name)
  (format t "Hi, ~a!~%" name))

(defun square (n)
  (* n n))

(defparameter *ages* '(("Alice" . 30) ("Bob" . 25)))

(let ((name "Alice")
      (x 5)
      (y 3.14)
      (active t)
      (nums '(1 2 3))
      (count 0))

  (greet name)
  (format t "Square of ~a is ~a~%" x (square x))

  (let ((p (make-point :x 3 :y 4)))
    (format t "Point: (~a, ~a)~%" (point-x p) (point-y p)))

  (dolist (n nums)
    (format t "~a " n))
  (format t "~%")

  (format t "Bob is ~a years old.~%" (cdr (assoc "Bob" *ages*)))

  (loop while (< count 3) do
    (format t "While loop: ~a~%" count)
    (incf count)))

