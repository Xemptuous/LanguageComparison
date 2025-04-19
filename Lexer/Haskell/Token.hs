-- module Token (Token (literal, token_type), TokenType, printToken) where
module Token where

data TokenType = Eof | Illegal | Let | Function | If | Else | Return | Identifier | Number | Boolean | Int | Bool | Asterisk | Slash | Dash | Plus | Equal | Lessthan | Greaterthan | Semicolon | Colon | Lparen | Rparen | Lbrace | Rbrace deriving (Enum, Show)

getTypeString :: TokenType -> String
getTypeString Eof = "Eof"
getTypeString Illegal = "Illegal"
getTypeString Let = "Let"
getTypeString Function = "Function"
getTypeString If = "If"
getTypeString Else = "Else"
getTypeString Return = "Return"
getTypeString Identifier = "Identifier"
getTypeString Number = "Number"
getTypeString Boolean = "Boolean"
getTypeString Int = "Int"
getTypeString Bool = "Bool"
getTypeString Asterisk = "Asterisk"
getTypeString Slash = "Slash"
getTypeString Dash = "Dash"
getTypeString Plus = "Plus"
getTypeString Equal = "Equal"
getTypeString Lessthan = "Lessthan"
getTypeString Greaterthan = "Greaterthan"
getTypeString Semicolon = "Semicolon"
getTypeString Colon = "Colon"
getTypeString Lparen = "Lparen"
getTypeString Rparen = "Rparen"
getTypeString Lbrace = "Lbrace"
getTypeString Rbrace = "Rbrace"

data Token = Token
  { literal :: String,
    token_type :: TokenType
  }

printToken token = do
  print $ "Token literal:(" ++ literal token ++ ") type:(" ++ getTypeString (token_type token) ++ ")"
