type point = {
  x: int,
  y: int
}

let greet = (name: string) => {
  Js.log("Hi, " ++ name ++ "!")
}

let square = (n: int): int => n * n

let name = "Alice"
let x = 5
let y = 3.14
let active = true

greet(name)
Js.log("Square of " ++ string_of_int(x) ++ " is " ++ string_of_int(square(x)))

let p = {x: 3, y: 4}
Js.log("(" ++ string_of_int(p.x) ++ ", " ++ string_of_int(p.y) ++ ")")

let nums = [1, 2, 3]
nums->List.iter(n => Js.log(n))

let ages = Js.Dict.fromList([("Alice", 30), ("Bob", 25)])
switch Js.Dict.get(ages, "Bob") {
| Some(b) => Js.log("Bob is " ++ string_of_int(b) ++ " years old.")
| None => ()
}

let rec loop = count =>
  if count < 3 {
    Js.log("While loop: " ++ string_of_int(count))
    loop(count + 1)
  }

loop(0)

