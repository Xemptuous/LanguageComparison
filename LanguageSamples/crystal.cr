struct Point
  property x, y
  def initialize(@x : Int32, @y : Int32); end
end

enum Color
  Red
  Green
  Blue
end

def greet(name)
  puts "Hi, #{name}!"
end

def square(n)
  n * n
end

name = "Alice"
x = 5
y = 3.14
active = true

greet(name)
puts "Square of #{x} is #{square(x)}"

p = Point.new(3, 4)
puts "Point: (#{p.x}, #{p.y})"

nums = [1, 2, 3]
nums.each { |n| print "#{n} " }
puts

ages = { "Alice" => 30, "Bob" => 25 }
puts "Bob is #{ages["Bob"]} years old."

count = 0
while count < 3
  puts "While loop: #{count}"
  count += 1
end

c = Color::Green
puts "Color is Green" if c == Color::Green

