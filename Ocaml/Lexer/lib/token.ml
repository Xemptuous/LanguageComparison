module StringMap = Map.Make (String)

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

let tokentype_to_string = function
  | EOF -> "EOF"
  | Illegal -> "Illegal"
  | Let -> "Let"
  | Const -> "Const"
  | Struct -> "Struct"
  | Function -> "Function"
  | If -> "If"
  | Else -> "Else"
  | Switch -> "Switch"
  | Case -> "Case"
  | Break -> "Break"
  | Return -> "Return"
  | While -> "While"
  | For -> "For"
  | And -> "And"
  | Or -> "Or"
  | In -> "In"
  | Identifier -> "Identifier"
  | Number -> "Number"
  | Float -> "Float"
  | Boolean -> "Boolean"
  | String -> "String"
  | Char -> "Char"
  | Int -> "Int"
  | F32 -> "F32"
  | Bool -> "Bool"
  | Str -> "Str"
  | Nil -> "Nil"
  | Void -> "Void"
  | Exclamation -> "Exclamation"
  | At -> "At"
  | Hashtag -> "Hashtag"
  | Dollar -> "Dollar"
  | Percent -> "Percent"
  | Caret -> "Caret"
  | Ampersand -> "Ampersand"
  | Asterisk -> "Asterisk"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | Minus -> "Minus"
  | Underscore -> "Underscore"
  | Plus -> "Plus"
  | Assign -> "Assign"
  | Lbracket -> "Lbracket"
  | Rbracket -> "Rbracket"
  | Lbrace -> "Lbrace"
  | Rbrace -> "Rbrace"
  | Semicolon -> "Semicolon"
  | Colon -> "Colon"
  | Apostrophe -> "Apostrophe"
  | Quote -> "Quote"
  | Comma -> "Comma"
  | Period -> "Period"
  | Lessthan -> "Lessthan"
  | Greaterthan -> "Greaterthan"
  | Slash -> "Slash"
  | Question -> "Question"
  | Backslash -> "Backslash"
  | Pipe -> "Pipe"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | PlusEq -> "PlusEq"
  | MinusEq -> "MinusEq"
  | MultEq -> "MultEq"
  | DivEq -> "DivEq"
  | LtEqual -> "LtEqual"
  | GtEqual -> "GtEqual"
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Comment -> "Comment"

let keywords =
  StringMap.of_seq
  @@ List.to_seq
       [
         ("let", Let);
         ("const", Const);
         ("struct", Struct);
         ("fn", Function);
         ("if", If);
         ("else", Else);
         ("switch", Switch);
         ("case", Case);
         ("break", Break);
         ("return", Return);
         ("while", While);
         ("for", For);
         ("and", And);
         ("or", Or);
         ("in", In);
         ("true", Boolean);
         ("false", Boolean);
         ("bool", Bool);
         ("int", Int);
         ("f32", F32);
         ("str", Str);
         ("nil", Nil);
         ("void", Void);
       ]

let double_token_map =
  StringMap.of_seq
  @@ List.to_seq
       [
         ("==", Equal);
         ("!=", NotEqual);
         ("==", Equal);
         ("!=", NotEqual);
         ("+=", PlusEq);
         ("-=", MinusEq);
         ("*=", MultEq);
         ("/=", DivEq);
         ("<=", LtEqual);
         (">=", GtEqual);
         ("++", Increment);
         ("--", Decrement);
         ("//", Comment);
         ("//", Comment);
       ]

let lookup_identifier ident =
  match StringMap.find_opt ident keywords with
  | Some kw -> kw
  | None -> Identifier

let _ = StringMap.of_seq @@ List.to_seq [ ("==", Equal) ]

type token = { literal : string; token_type : token_type }
type lexer = { input : string; curr : int; peek : int; char : char option }

let advance lex =
  let newLexer = lex in
  if lex.peek >= String.length lex.input then { newLexer with char = None }
  else
    {
      newLexer with
      curr = lex.peek;
      peek = lex.peek + 1;
      char = Some (String.get lex.input lex.peek);
    }

let print_token tok =
  Printf.printf "Token{literal:\"%s\" type: %s}\n" tok.literal
    (tokentype_to_string tok.token_type)

let read_while pred lex =
  let buf = Buffer.create 16 in
  let rec loop l =
    match l.char with
    | Some c when pred c ->
        Buffer.add_char buf c;
        loop (advance l)
    | _ -> (Buffer.contents buf, l)
  in
  loop lex

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let is_digit c =
  let code = Char.code c in
  (code >= Char.code '0' && code <= Char.code '9') || code == Char.code '.'

let is_letter c =
  let code = Char.code c in
  (code >= Char.code 'a' && code <= Char.code 'z')
  || (code >= Char.code 'A' && code <= Char.code 'Z')
  || is_digit c || c = '_'

let count_substring str sub =
  let sub_len = String.length sub in
  let len_diff = String.length str - sub_len and reg = Str.regexp_string sub in
  let rec aux i n =
    if i > len_diff then n
    else
      try
        let pos = Str.search_forward reg str i in
        aux (pos + sub_len) (succ n)
      with Not_found -> n
  in
  aux 0 0

let read_comment lex = read_while (fun c -> c <> '\n') (advance lex)
let read_string lex = read_while (fun c -> c <> '"') lex
let read_char lex = read_while (fun c -> c <> '\'') lex
let read_identifier lex = read_while is_letter lex

let read_number lex =
  let digits, l' = read_while is_digit lex in
  match count_substring digits "." with
  | 0 -> (Number, digits, l')
  | 1 -> (Float, digits, l')
  | _ -> (Illegal, digits, l')

let rec next_token lex =
  let ds =
    if lex.curr + 1 < String.length lex.input then
      String.sub lex.input lex.curr 2
    else ""
  in

  match lex.char with
  | Some ch when is_whitespace ch -> next_token (advance lex)
  | _ when ds = "//" ->
      let ident, new_lexer = read_comment lex in
      ({ literal = ident; token_type = Comment }, new_lexer)
  | _ when StringMap.mem ds double_token_map ->
      let ttype = StringMap.find ds double_token_map in
      ({ literal = ds; token_type = ttype }, lex |> advance |> advance)
  | Some '"' ->
      let str, l' = read_string (advance lex) in
      ({ literal = str; token_type = String }, advance l')
  | Some '\'' ->
      let ch_str, l' = read_char (advance lex) in
      ({ literal = ch_str; token_type = Char }, advance l')
  | Some '!' -> ({ literal = "!"; token_type = Exclamation }, advance lex)
  | Some '@' -> ({ literal = "@"; token_type = At }, advance lex)
  | Some '#' -> ({ literal = "#"; token_type = Hashtag }, advance lex)
  | Some '$' -> ({ literal = "$"; token_type = Dollar }, advance lex)
  | Some '%' -> ({ literal = "%"; token_type = Percent }, advance lex)
  | Some '^' -> ({ literal = "^"; token_type = Caret }, advance lex)
  | Some '&' -> ({ literal = "&"; token_type = Ampersand }, advance lex)
  | Some '*' -> ({ literal = "*"; token_type = Asterisk }, advance lex)
  | Some '(' -> ({ literal = "("; token_type = Lparen }, advance lex)
  | Some ')' -> ({ literal = ")"; token_type = Rparen }, advance lex)
  | Some '-' -> ({ literal = "-"; token_type = Minus }, advance lex)
  | Some '_' -> ({ literal = "_"; token_type = Underscore }, advance lex)
  | Some '+' -> ({ literal = "+"; token_type = Plus }, advance lex)
  | Some '=' -> ({ literal = "="; token_type = Assign }, advance lex)
  | Some '[' -> ({ literal = "["; token_type = Lbracket }, advance lex)
  | Some ']' -> ({ literal = "]"; token_type = Rbracket }, advance lex)
  | Some '{' -> ({ literal = "{"; token_type = Lbrace }, advance lex)
  | Some '}' -> ({ literal = "}"; token_type = Rbrace }, advance lex)
  | Some ';' -> ({ literal = ";"; token_type = Semicolon }, advance lex)
  | Some ':' -> ({ literal = ":"; token_type = Colon }, advance lex)
  | Some ',' -> ({ literal = ","; token_type = Comma }, advance lex)
  | Some '.' -> ({ literal = "."; token_type = Period }, advance lex)
  | Some '<' -> ({ literal = "<"; token_type = Lessthan }, advance lex)
  | Some '>' -> ({ literal = ">"; token_type = Greaterthan }, advance lex)
  | Some '/' -> ({ literal = "/"; token_type = Slash }, advance lex)
  | Some '?' -> ({ literal = "?"; token_type = Question }, advance lex)
  | Some '\\' -> ({ literal = "\\"; token_type = Backslash }, advance lex)
  | Some '|' -> ({ literal = "|"; token_type = Pipe }, advance lex)
  | Some ch when is_digit ch ->
      let ttype, num, l' = read_number lex in
      ({ literal = num; token_type = ttype }, l')
  | Some ch when is_letter ch ->
      let ident, l' = read_identifier lex in
      let ttype = lookup_identifier ident in
      ({ literal = ident; token_type = ttype }, l')
  | None -> ({ literal = ""; token_type = EOF }, advance lex)
  | Some ch ->
      ({ literal = String.make 1 ch; token_type = Illegal }, advance lex)
