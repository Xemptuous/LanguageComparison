-module(lexer).

-export([new/1, next_token/1, print_token/1, parse_while/1]).

-record(token, {type, literal = ""}).
-record(lexer, {input, curr = 0, peek = 0, ch}).

-define(SINGLE_TOKEN_MAP,
        #{$+ => plus,
          $- => dash,
          $* => asterisk,
          $+ => plus,
          $* => asterisk,
          $/ => slash,
          $- => dash,
          $+ => plus,
          $= => equal,
          $< => lessthan,
          $> => greaterthan,
          $; => semicolon,
          $: => colon,
          $( => lparen,
          $) => rparen,
          ${ => lbrace,
          $} => rbrace}).
-define(IDENTIFIER_MAP,
        #{"let" => let_,
          "fn" => function,
          "if" => if_,
          "else" => else_,
          "return" => return,
          "true" => boolean,
          "false" => boolean,
          "bool" => bool,
          "int" => int}).

new(Input) ->
    advance(#lexer{input = Input}).

parse_while(L) ->
    {T = #token{type = Type}, NewL} = next_token(L),
    case Type of
        eof ->
            ok;
        _ ->
            print_token(T),
            parse_while(NewL)
    end.

advance(L = #lexer{input = Input, peek = P}) ->
    case P >= length(Input) of
        true ->
            L#lexer{ch = eof};
        false ->
            L#lexer{curr = P,
                    peek = P + 1,
                    ch = lists:nth(P + 1, Input)}
    end.

next_token(L = #lexer{ch = Ch}) ->
    case maps:get(Ch, ?SINGLE_TOKEN_MAP, undefined) of
        undefined ->
            next_token_fallback(L);
        TokenType ->
            {#token{type = TokenType, literal = <<Ch>>}, advance(L)}
    end.

next_token_fallback(L = #lexer{ch = C}) ->
    if C == eof ->
           {#token{type = eof}, L};
       C =< 32 -> % whitespace
           next_token(advance(L));
       % identifier
       C >= $a, C =< $z; C >= $A, C =< $Z; C == $_ ->
           {Ident, NewL} = read_identifier(L),
           case maps:get(Ident, ?IDENTIFIER_MAP, undefined) of
               undefined ->
                   {#token{type = identifier, literal = Ident}, NewL};
               Type ->
                   {#token{type = Type, literal = Ident}, NewL}
           end;
       % number
       C >= $0, C =< $9 ->
           {Num, NewL} = read_number(L),
           {#token{type = number, literal = Num}, NewL};
       true ->
           {#token{type = illegal, literal = <<C>>}, advance(L)}
    end.

read_while(L = #lexer{ch = Ch}, Acc, Pred) ->
    case Pred(Ch) of
        true ->
            NewL = advance(L),
            read_while(NewL, [Ch | Acc], Pred);
        false ->
            {lists:reverse(Acc), L}
    end.

read_identifier(L) ->
    read_while(L, [], fun is_letter/1).

read_number(L) ->
    read_while(L, [], fun is_digit/1).

is_letter(C) ->
    C >= $a andalso C =< $z orelse C >= $A andalso C =< $Z orelse C == $_.

is_digit(C) ->
    C >= $0 andalso C =< $9.

print_token(T = #token{literal = L}) ->
    case is_number(L) of
        true ->
            io:format("Token literal: \"~c\" type: ~s~n", [T#token.literal, T#token.type]);
        false ->
            io:format("Token literal: \"~s\" type: ~s~n", [T#token.literal, T#token.type])
    end.
