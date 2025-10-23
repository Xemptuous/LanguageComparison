module Lexer

open System

type op =
    | Add
    | Sub
    | Mul
    | Div
    | Pow
    | Mod

type token =
    | Number of float
    | Identifier of string
    | Operator of op
    | LParen
    | RParen

let precedence op =
    match op with
    | Add
    | Sub -> 1
    | Mul
    | Div
    | Mod -> 2
    | Pow -> 3

let is_left_associative op =
    match op with
    | Pow -> false
    | _ -> true

let push tokens token = Array.append tokens [| token |]

let lex (input: string) : token[] =
    let mutable tokens = [||]
    let mutable i = 0

    while i < input.Length do
        match input.[i] with
        | c when Char.IsWhiteSpace c -> i <- i + 1
        | c when Char.IsDigit c ->
            let start = i

            while i < input.Length && (Char.IsDigit input.[i] || input.[i] = '.') do
                i <- i + 1

            let num = input[start .. i - 1]

            if num.Contains "." then
                tokens <- push tokens (Number(float num))
            else
                tokens <- push tokens (Number(int num))
        | '+' ->
            tokens <- push tokens (Operator Add)
            i <- i + 1
        | '-' ->
            tokens <- push tokens (Operator Sub)
            i <- i + 1
        | '*' ->
            tokens <- push tokens (Operator Mul)
            i <- i + 1
        | '/' ->
            tokens <- push tokens (Operator Div)
            i <- i + 1
        | '^' ->
            tokens <- push tokens (Operator Pow)
            i <- i + 1
        | '%' ->
            tokens <- push tokens (Operator Mod)
            i <- i + 1
        | '(' ->
            tokens <- push tokens LParen
            i <- i + 1
        | ')' ->
            tokens <- push tokens RParen
            i <- i + 1
        | _ -> i <- i + 1

    tokens
