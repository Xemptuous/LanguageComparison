module Evaluator

open System
open System.Collections.Generic
open Lexer

let calculate (tokens: token[]) =
    let mutable stack = Stack()

    for token in tokens do
        match token with
        | Number n -> stack.Push n
        | Operator o ->
            let r = stack.Pop()
            let l = stack.Pop()

            stack.Push(
                match o with
                | Add -> l + r
                | Sub -> l - r
                | Mul -> l * r
                | Div -> l / r
                | Pow -> Math.Pow(l, r)
                | Mod -> l % r
            )
        | _ -> ()

    stack.Pop()
