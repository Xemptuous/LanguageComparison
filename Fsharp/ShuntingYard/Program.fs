let input = "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3"

let lexed = Lexer.lex input
let rpn = Parser.to_rpn lexed
let result = Evaluator.calculate rpn

printfn "Result: %f" result
