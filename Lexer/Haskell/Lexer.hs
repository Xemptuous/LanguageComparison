module Lexer where

import Data.Char (isDigit, isLetter, isSpace)
import Data.Map (Map, fromList)
import Data.Map qualified as M
import Data.Maybe (fromJust, isNothing)
import Token
import Token qualified as T

data Lexer = Lexer
  { input :: String,
    curr :: Int,
    peek :: Int,
    char :: Maybe Char
  }
  deriving (Show)

newLexer :: String -> Lexer
newLexer input = advance (Lexer input 0 0 Nothing)

parseWhile :: Lexer -> IO ()
parseWhile lexer =
  let (tok, newLexer) = nextToken lexer
   in if token_type tok == Eof
        then return ()
        else print tok >> parseWhile newLexer

nextToken :: Lexer -> (Token, Lexer)
nextToken lexer@(Lexer _ _ _ (Just ch)) =
  case M.lookup ch T.tokenMap of
    Just ttype -> (Token [ch] ttype, advance lexer)
    Nothing -> nextTokenFallback lexer

nextTokenFallback :: Lexer -> (Token, Lexer)
nextTokenFallback lexer@(Lexer _ _ _ Nothing) =
  (Token "" Eof, lexer)
nextTokenFallback lexer@(Lexer _ _ _ (Just ch))
  | isSpace ch = nextToken (advance lexer) -- Replace M.empty with your map
  | isLetter ch || ch == '_' =
      let (ident, newL) = readIdentifier lexer
          tokType = M.findWithDefault Identifier ident keywordMap
       in (Token ident tokType, newL)
  | isDigit ch =
      let (num, newL) = readNumber lexer
       in (Token num Number, newL)
  | otherwise =
      (Token [ch] Illegal, advance lexer)

readWhile :: (Char -> Bool) -> Lexer -> (String, Lexer)
readWhile predicate = go []
  where
    go acc lexer@(Lexer _ _ _ (Just ch))
      | predicate ch = go (ch : acc) (advance lexer)
    go acc lexer = (reverse acc, lexer)

readIdentifier :: Lexer -> (String, Lexer)
readIdentifier = readWhile isIdentChar
  where
    isIdentChar c = isLetter c || c == '_'

readNumber :: Lexer -> (String, Lexer)
readNumber = readWhile isDigit

advance :: Lexer -> Lexer
advance lexer@(Lexer input _ p _)
  | p >= length input = lexer {char = Nothing}
  | otherwise =
      lexer
        { curr = p,
          peek = p + 1,
          char = Just (input !! p)
        }
