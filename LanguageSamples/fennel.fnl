(local Point {:new (fn [x y] {:x x :y y})
              :show (fn [self] (print (.. "(" self.x ", " self.y ")"))) })

(fn greet [name]
  (print (.. "Hi, " name "!")))

(fn square [n] (* n n))

(local name "Alice")
(local x 5)
(local y 3.14)
(local active true)

(greet name)
(print "Square of" x "is" (square x))

(local p (Point.new 3 4))
(Point.show p)

(each [_ n (ipairs [1 2 3])]
  (io.write (.. n " ")))
(print)

(local ages {:Alice 30 :Bob 25})
(print "Bob is" ages.Bob "years old.")

(var count 0)
(while (< count 3)
  (print "While loop:" count)
  (+= count 1))

