import Lexer (newLexer, parseWhile)

main :: IO ()
main = do
  let input =
        "fn is_gt_ten(num: int) bool {\n\
        \ if (num > 10) {\n\
        \ return true;\n\
        \ } else {\n\
        \ return false;\n\
        \ }\n\
        \ }\n\
        \ let ten = 5 + 5 * 4 / 2 - 5;\n\
        \ print(is_big(ten));"

  parseWhile (newLexer input)
