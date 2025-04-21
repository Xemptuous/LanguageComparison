type Point = { x: int; y: int }

let greet name = printfn "Hi, %s!" name
let square n = n * n

let name = "Alice"
let x = 5
let y = 3.14
let active = true

greet name
printfn "Square of %d is %d" x (square x)

let p = { x = 3; y = 4 }
printfn "Point: (%d, %d)" p.x p.y

let nums = [1; 2; 3]
nums |> List.iter (printf "%d ")
printfn ""

let ages = Map.ofList [("Alice", 30); ("Bob", 25)]
printfn "Bob is %d years old." (ages.["Bob"])

let mutable count = 0
while count < 3 do
    printfn "While loop: %d" count
    count <- count + 1

