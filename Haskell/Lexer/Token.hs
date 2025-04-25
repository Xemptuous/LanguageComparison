-- module Token (Token (literal, token_type), TokenType, printToken) where
module Token where

import Data.Map qualified as M

data Token = Token
  { literal :: String,
    token_type :: TokenType
  }
  deriving (Show)

data TokenType
  = Eof
  | Illegal
  | Let
  | Const
  | Struct
  | Function
  | If
  | Else
  | Switch
  | Case
  | Break
  | Return
  | While
  | For
  | And
  | Or
  | In
  | Identifier
  | Number
  | Float
  | Boolean
  | String
  | Char
  | Int
  | F32
  | Bool
  | Str
  | Nil
  | Void
  | Exclamation
  | At
  | Hashtag
  | Dollar
  | Percent
  | Caret
  | Ampersand
  | Asterisk
  | Lparen
  | Rparen
  | Minus
  | Underscore
  | Plus
  | Assign
  | Lbracket
  | Rbracket
  | Lbrace
  | Rbrace
  | Semicolon
  | Colon
  | Apostrophe
  | Quote
  | Comma
  | Period
  | Lessthan
  | Greaterthan
  | Slash
  | Question
  | Backslash
  | Pipe
  | Equal
  | NotEqual
  | PlusEq
  | MinusEq
  | MultEq
  | DivEq
  | LtEqual
  | GtEqual
  | Increment
  | Decrement
  | Comment
  deriving (Show, Eq)

type TokenMap = M.Map Char TokenType

singleTokenMap :: TokenMap
singleTokenMap =
  M.fromList
    [ ('!', Exclamation),
      ('@', At),
      ('#', Hashtag),
      ('$', Dollar),
      ('%', Percent),
      ('^', Caret),
      ('&', Ampersand),
      ('*', Asterisk),
      ('(', Lparen),
      (')', Rparen),
      ('-', Minus),
      ('_', Underscore),
      ('+', Plus),
      ('=', Assign),
      ('[', Lbracket),
      (']', Rbracket),
      ('{', Lbrace),
      ('}', Rbrace),
      (';', Semicolon),
      (':', Colon),
      ('\'', Char),
      ('"', String),
      (',', Comma),
      ('.', Period),
      ('<', Lessthan),
      ('>', Greaterthan),
      ('/', Slash),
      ('?', Question),
      ('\\', Backslash),
      ('|', Pipe)
    ]

doubleTokenMap :: M.Map String TokenType
doubleTokenMap =
  M.fromList
    [ ("==", Equal),
      ("!=", NotEqual),
      ("+=", PlusEq),
      ("-=", MinusEq),
      ("*=", MultEq),
      ("/=", DivEq),
      ("<=", LtEqual),
      (">=", GtEqual),
      ("++", Increment),
      ("--", Decrement),
      ("//", Comment)
    ]

type KeywordMap = M.Map String TokenType

keywordMap :: KeywordMap
keywordMap =
  M.fromList
    [ ("let", Let),
      ("const", Const),
      ("struct", Struct),
      ("fn", Function),
      ("if", If),
      ("else", Else),
      ("switch", Switch),
      ("case", Case),
      ("break", Break),
      ("return", Return),
      ("while", While),
      ("for", For),
      ("and", And),
      ("or", Or),
      ("in", In),
      ("true", Boolean),
      ("false", Boolean),
      ("bool", Bool),
      ("int", Int),
      ("f32", F32),
      ("str", Str),
      ("nil", Nil),
      ("void", Void)
    ]
