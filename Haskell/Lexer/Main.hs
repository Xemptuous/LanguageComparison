import Lexer
import Token

main :: IO ()
main = do
  let input =
        "struct Node {\n\
        \    data: int;\n\
        \    left: *Node;\n\
        \    right: *Node;\n\
        \}\n\
        \fn add_nums(x: int, y: int) int {\n\
        \    return x + y;\n\
        \}\n\
        \\n\
        \// check if str starts with an h\n\
        \fn starts_with_h(s: str) bool {\n\
        \    return s[0] == 'H';\n\
        \}\n\
        \\n\
        \fn printNum(n: int|f32) void {\n\
        \    print(n);\n\
        \}\n\
        \let rootNode = Node{5, nil, nil};\n\
        \print(rootNode.data);\n\
        \\n\
        \const ten: int = 5 + 5 * 4 / 2 - 5;\n\
        \const newNum = add_nums(ten, 25);\n\
        \const isTen = ten == 10 ? true : false;\n\
        \const nothing = nil;\n\
        \\n\
        \let mystring: str = \"Hello, Lexer!\";\n\
        \for ch: str in mystring {\n\
        \    print(ch); // a comment here!\n\
        \}\n\
        \\n\
        \const myfloat: f32 = 69.420;\n\
        \\n\
        \let counter = 0; \n\
        \let otherCounter = 20; \n\
        \\n\
        \while counter < 10 or otherCounter > 10 {\n\
        \    otherCounter -= 1;\n\
        \    counter++;\n\
        \}"

  parseWhile (newLexer input)

parseWhile :: Lexer -> IO ()
parseWhile lexer =
  let (tok, newLexer) = nextToken lexer
   in if token_type tok == Eof
        then return ()
        else print tok >> parseWhile newLexer
