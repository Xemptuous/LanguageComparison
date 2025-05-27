module Lexer where

import Data.ByteString.Char8 qualified as C
import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.Map qualified as M
import Token

data Lexer = Lexer C.ByteString Int Int deriving (Show)

advance :: Lexer -> Lexer
advance (Lexer i _ p)
  | p >= C.length i = Lexer i p p
  | otherwise = Lexer i p (succ p)

advance2 :: Lexer -> Lexer
advance2 lexer = advance $ advance lexer

currentChar :: Lexer -> Char
currentChar (Lexer input curr _) =
  if curr >= C.length input then '\0' else C.index input curr

-- if curr >= length input then '\0' else input !! curr

peekChar :: Lexer -> Char
peekChar (Lexer input _ peek) =
  if peek >= C.length input then '\0' else C.index input peek

parse :: Lexer -> (Token, Lexer)
parse lexer
  | isSpace ch = parse $ advance lexer
  | isAlpha ch = readIdentifier lexer []
  | isNumber ch = readNumber lexer []
  | ch == '=' = case peek of
      '=' -> (Token Equal [ch, peek], advance2 lexer)
      _ -> (Token Assign [ch], advance lexer)
  | ch == '{' = (Token Lbrace [ch], advance lexer)
  | ch == '}' = (Token Rbrace [ch], advance lexer)
  | ch == ';' = (Token Semicolon [ch], advance lexer)
  | ch == ':' = (Token Colon [ch], advance lexer)
  | ch == '(' = (Token Lparen [ch], advance lexer)
  | ch == ')' = (Token Rparen [ch], advance lexer)
  | ch == '[' = (Token Lbracket [ch], advance lexer)
  | ch == ']' = (Token Rbracket [ch], advance lexer)
  | ch == ',' = (Token Comma [ch], advance lexer)
  | ch == '.' = (Token Period [ch], advance lexer)
  | ch == '\'' = readChar (advance lexer) ""
  | ch == '"' = readString (advance lexer) ""
  | ch == '!' = case peek of
      '=' -> (Token NotEqual [ch, peek], advance2 lexer)
      _ -> (Token Exclamation [ch], advance lexer)
  | ch == '*' = case peek of
      '=' -> (Token MultEq [ch, peek], advance2 lexer)
      _ -> (Token Asterisk [ch], advance lexer)
  | ch == '-' = case peek of
      '=' -> (Token MinusEq [ch, peek], advance2 lexer)
      '-' -> (Token Decrement [ch, peek], advance2 lexer)
      _ -> (Token Minus [ch], advance lexer)
  | ch == '+' = case peek of
      '=' -> (Token PlusEq [ch, peek], advance2 lexer)
      '+' -> (Token Increment [ch, peek], advance2 lexer)
      _ -> (Token Plus [ch], advance lexer)
  | ch == '<' = case peek of
      '=' -> (Token LtEqual [ch, peek], advance2 lexer)
      _ -> (Token Lessthan [ch], advance lexer)
  | ch == '>' = case peek of
      '=' -> (Token GtEqual [ch, peek], advance2 lexer)
      _ -> (Token Greaterthan [ch], advance lexer)
  | ch == '/' = case peek of
      '=' -> (Token DivEq [ch, peek], advance2 lexer)
      '/' -> readComment lexer ""
      _ -> (Token Slash [ch], advance lexer)
  | ch == '?' = (Token Question [ch], advance lexer)
  | ch == '%' = (Token Percent [ch], advance lexer)
  | ch == '\\' = (Token Backslash [ch], advance lexer)
  | ch == '_' = (Token Underscore [ch], advance lexer)
  | ch == '^' = (Token Caret [ch], advance lexer)
  | ch == '&' = (Token Ampersand [ch], advance lexer)
  | ch == '|' = (Token Pipe [ch], advance lexer)
  | ch == '@' = (Token At [ch], advance lexer)
  | ch == '#' = (Token Hashtag [ch], advance lexer)
  | ch == '$' = (Token Dollar [ch], advance lexer)
  | ch == '\0' = (Token EOF [ch], lexer)
  | otherwise = (Token Illegal [ch], lexer)
  where
    ch = currentChar lexer
    peek = peekChar lexer

readWhile :: Lexer -> String -> (Char -> Bool) -> (String, Lexer)
readWhile lexer acc f
  | x == '\0' = (acc, lexer)
  | f x = readWhile (advance lexer) (x : acc) f
  | otherwise = (reverse acc, lexer)
  where
    x = currentChar lexer

readIdentifier :: Lexer -> String -> (Token, Lexer)
readIdentifier lexer acc =
  let (ident, newLex) = readWhile lexer acc isIdent
      tokType = M.findWithDefault Identifier ident keywordMap
   in (Token tokType ident, newLex)
  where
    isIdent c = isAlphaNum c || c == '_'

readNumber :: Lexer -> String -> (Token, Lexer)
readNumber lexer acc =
  let (num, newLex) = readWhile lexer acc isNum
   in case count '.' num of
        0 -> (Token Number num, newLex)
        1 -> (Token Float num, newLex)
        _ -> (Token Illegal num, newLex)
  where
    isNum c = isNumber c || c == '.'

readString :: Lexer -> String -> (Token, Lexer)
readString lexer acc =
  let (str, newLex) = readWhile lexer acc isString
   in (Token String str, advance newLex)
  where
    isString c = c /= '"'

readChar :: Lexer -> String -> (Token, Lexer)
readChar lexer acc =
  let (str, newLex) = readWhile lexer acc isChar
   in (Token Char str, advance newLex)
  where
    isChar c = c /= '\''

readComment :: Lexer -> String -> (Token, Lexer)
readComment lexer acc =
  let (str, newLex) = readWhile lexer acc isComment
   in (Token Comment str, newLex)
  where
    isComment c = c `notElem` ['\r', '\n']

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)
