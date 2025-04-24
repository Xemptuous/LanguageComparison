(defrecord Point [x y])

(defn greet [name]
  (println "Hi," name "!"))

(defn square [n]
  (* n n))

(def ages { "Alice" 30, "Bob" 25 })

(let [name "Alice"
      x 5
      y 3.14
      active true
      nums [1 2 3]
      p (->Point 3 4)]

  (greet name)
  (println "Square of" x "is" (square x))
  (println "Point:" (:x p) (:y p))
  (doseq [n nums] (print n " "))
  (println)
  (println "Bob is" (ages "Bob") "years old.")
  (loop [count 0]
    (when (< count 3)
      (println "While loop:" count)
      (recur (inc count)))))

