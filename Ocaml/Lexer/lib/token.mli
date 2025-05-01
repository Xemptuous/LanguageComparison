module StringMap : Map.S with type key = string

type token_type =
  | EOF
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

type token = { literal : string; token_type : token_type }
type lexer = { input : string; curr : int; peek : int; char : char option }

val tokentype_to_string : token_type -> string
val advance : lexer -> lexer
val print_token : token -> unit

(* Character predicates *)
val is_whitespace : char -> bool
val is_letter : char -> bool
val is_digit : char -> bool

(* Read functions *)
val read_while : (char -> bool) -> lexer -> string * lexer

(* val read_identifier : lexer -> string * lexer *)
(* val read_number : lexer -> string * lexer *)

(* Keyword map and lookup *)
val keywords : token_type StringMap.t
val lookup_identifier : string -> token_type

(* Token generation *)
val next_token : lexer -> token * lexer
