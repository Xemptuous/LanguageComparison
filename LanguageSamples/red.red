Red []

greet: func [name][print ["Hi," name "!"]]
square: func [n][n * n]

name: "Alice"
x: 5
y: 3.14
active: true

greet name
print ["Square of" x "is" square x]

point: context [x: 3 y: 4]
print [ "(" point/x "," point/y ")" ]

nums: [1 2 3]
foreach n nums [prin [n " "]]
print ""

ages: make map! [ "Alice" 30 "Bob" 25 ]
print ["Bob is" ages/Bob "years old."]

count: 0
while [count < 3][
  print ["While loop:" count]
  count: count + 1
]

