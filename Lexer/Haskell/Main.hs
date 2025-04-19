import Token

main :: IO ()
main = do
  let tok = Token {literal = "let", token_type = Let}
  printToken tok
  let tok = Token {literal = "abc", token_type = Identifier}
  printToken tok
  let tok = Token {literal = "=", token_type = Equal}
  printToken tok
  let tok = Token {literal = "5", token_type = Number}
  printToken tok
  let tok = Token {literal = ";", token_type = Semicolon}
  printToken tok
