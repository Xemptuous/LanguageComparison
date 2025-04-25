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

advance :: Lexer -> Lexer
advance lexer@(Lexer input _ p _)
  | p >= length input = lexer {char = Nothing}
  | otherwise =
      lexer
        { curr = p,
          peek = p + 1,
          char = Just (input !! p)
        }

nextToken :: Lexer -> (Token, Lexer)
nextToken lexer@(Lexer _ _ _ Nothing) =
  (Token "" Eof, lexer)
nextToken lexer@(Lexer input curr peek (Just ch)) = do
  let ds = take 2 $ drop curr input :: String

  case M.lookup ds T.doubleTokenMap of
    Just ttype -> do
      let newL = advance lexer
      if ttype == Comment
        then do
          let (c, newL) = readComment lexer
          (Token c Comment, newL)
        else (Token ds ttype, advance newL)
    Nothing -> do
      case ch of
        '"' ->
          let (str, newL) = readString (advance lexer)
           in (Token str String, advance newL)
        '\'' ->
          let (str, newL) = readChar (advance lexer)
           in (Token str Char, advance newL)
        _ ->
          case M.lookup ch T.singleTokenMap of
            Just ttype -> (Token [ch] ttype, advance lexer)
            Nothing -> nextTokenFallback lexer

nextTokenFallback :: Lexer -> (Token, Lexer)
nextTokenFallback lexer@(Lexer _ _ _ Nothing) =
  (Token "" Eof, lexer)
nextTokenFallback lexer@(Lexer _ _ _ (Just ch))
  | ch == '"' =
      let (str, newL) = readString (advance lexer)
       in (Token str String, newL)
  | ch == '\'' =
      let (str, newL) = readChar (advance lexer)
       in (Token str Char, newL)
  | isSpace ch = nextToken (advance lexer)
  | isLetter ch || ch == '_' =
      let (ident, newL) = readIdentifier lexer
          tokType = M.findWithDefault Identifier ident keywordMap
       in (Token ident tokType, newL)
  | isDigit ch = do
      let (num, newL) = readNumber lexer
      case count num '.' of
        0 -> (Token num Number, newL)
        1 -> (Token num Float, newL)
        _ -> (Token num Illegal, newL)
  | otherwise =
      (Token [ch] Illegal, advance lexer)

readWhile :: (Char -> Bool) -> Lexer -> (String, Lexer)
readWhile predicate = go []
  where
    go acc lexer@(Lexer _ _ _ (Just ch))
      | predicate ch = go (ch : acc) (advance lexer)
    go acc lexer = (reverse acc, lexer)

readIdentifier :: Lexer -> (String, Lexer)
readIdentifier = readWhile isIdentChar where isIdentChar c = isLetter c || c == '_'

readString :: Lexer -> (String, Lexer)
readString = readWhile isNotQuote where isNotQuote c = c /= '"'

readChar :: Lexer -> (String, Lexer)
readChar = readWhile isNotApostrophe where isNotApostrophe c = c /= '\''

readComment :: Lexer -> (String, Lexer)
readComment = readWhile isNotNewline where isNotNewline c = c /= '\r' && c /= '\n'

readNumber :: Lexer -> (String, Lexer)
readNumber = readWhile isNum where isNum c = isDigit c || c == '.'

count xs find = length (filter (== find) xs)
