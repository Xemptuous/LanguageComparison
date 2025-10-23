module Parser

open System.Collections.Generic
open Lexer

let to_rpn (tokens: token[]) =
    let mutable out = Queue<token>()
    let mutable ops = Stack<token>()

    for token in tokens do
        match token with
        | Number n -> out.Enqueue(Number n)
        | Operator o1 ->
            let mutable continueLooping = true

            while continueLooping && ops.Count > 0 do
                match ops.Peek() with
                | Operator o2 when
                    precedence o2 > precedence o1
                    || precedence o2 = precedence o1 && is_left_associative o1
                    ->
                    out.Enqueue(ops.Pop())
                | _ -> continueLooping <- false

            ops.Push(Operator o1)
        | LParen -> ops.Push LParen
        | RParen ->
            while ops.Count > 0 && ops.Peek() <> LParen do
                out.Enqueue(ops.Pop())

            if ops.Count > 0 then
                ops.Pop() |> ignore

    while ops.Count > 0 do
        out.Enqueue(ops.Pop())

    out.ToArray()
