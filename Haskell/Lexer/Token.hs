-- module Token (Token (literal, token_type), TokenType, printToken) where
module Token where

import Data.Map qualified as M

data Token = Token
  { literal :: String,
    token_type :: TokenType
  }
  deriving (Show)

data TokenType = Eof | Illegal | Let | Function | If | Else | Return | Identifier | Number | Boolean | Int | Bool | Asterisk | Slash | Dash | Plus | Equal | Lessthan | Greaterthan | Semicolon | Colon | Lparen | Rparen | Lbrace | Rbrace deriving (Show, Eq)

type TokenMap = M.Map Char TokenType

tokenMap :: TokenMap
tokenMap =
  M.fromList
    [ ('+', Plus),
      ('-', Dash),
      ('*', Asterisk),
      ('+', Plus),
      ('*', Asterisk),
      ('/', Slash),
      ('-', Dash),
      ('+', Plus),
      ('=', Equal),
      ('<', Lessthan),
      ('>', Greaterthan),
      (';', Semicolon),
      (':', Colon),
      ('(', Lparen),
      (')', Rparen),
      ('{', Lbrace),
      ('}', Rbrace)
    ]

type KeywordMap = M.Map String TokenType

keywordMap :: KeywordMap
keywordMap =
  M.fromList
    [ ("let", Let),
      ("fn", Function),
      ("if", If),
      ("else", Else),
      ("return", Return),
      ("true", Boolean),
      ("false", Boolean),
      ("bool", Bool),
      ("int", Int)
    ]
