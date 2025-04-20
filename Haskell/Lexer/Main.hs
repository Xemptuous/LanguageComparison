import Lexer
import Token

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

parseWhile :: Lexer -> IO ()
parseWhile lexer =
  let (tok, newLexer) = nextToken lexer
   in if token_type tok == Eof
        then return ()
        else print tok >> parseWhile newLexer
