set name "Alice"
set x 5
set y 3.14
set active 1

proc greet {name} {
    puts "Hi, $name!"
}

proc square {n} {
    return [expr {$n * $n}]
}

greet $name
puts "Square of $x is [square $x]"

array set point [list x 3 y 4]
puts "($point(x), $point(y))"

set nums {1 2 3}
foreach n $nums {
    puts -nonewline "$n "
}
puts ""

array set ages [list Alice 30 Bob 25]
puts "Bob is $ages(Bob) years old."

set count 0
while {$count < 3} {
    puts "While loop: $count"
    incr count
}

