open System.Collections.Generic

let input =
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

let jump =
    let jump = Dictionary<int, int>()
    let stack = Stack()

    for i in 0 .. input.Length - 1 do
        match input.[i] with
        | '[' -> stack.Push i
        | ']' ->
            let idx = stack.Pop()
            jump.Add(i, idx)
            jump.Add(idx, i)
        | _ -> ()

    jump

let tape = Array.create 30_000 0
let mutable ip = 0
let mutable dp = 0

while ip < input.Length do
    match input.[ip] with
    | '>' -> dp <- dp + 1
    | '<' -> dp <- dp - 1
    | '+' -> tape.[dp] <- tape.[dp] + 1
    | '-' -> tape.[dp] <- tape.[dp] - 1
    | '.' -> printf "%c" (char tape.[dp])
    | '[' ->
        if tape[dp] = 0 then
            ip <- jump[ip]
    | ']' ->
        if tape[dp] <> 0 then
            ip <- jump[ip]
    | _ -> ()

    ip <- ip + 1
