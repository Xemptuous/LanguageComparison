-module(lexer).

-export([new/1, next_token/1, print_token/1, parse_while/1]).

-record(token, {type, literal = ""}).
-record(lexer, {input, curr = 0, peek = 0, ch}).

new(Input) ->
    L = #lexer{input = Input},
    advance(L).

parse_while(L) ->
    {T = #token{type = Type}, NewL} = next_token(L),
    case Type of
        eof ->
            ok;
        whitespace ->
            parse_while(NewL);
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

next_token(L = #lexer{ch = $*}) ->
    {#token{type = asterisk, literal = "*"}, advance(L)};
next_token(L = #lexer{ch = $/}) ->
    {#token{type = slash, literal = "/"}, advance(L)};
next_token(L = #lexer{ch = $-}) ->
    {#token{type = dash, literal = "-"}, advance(L)};
next_token(L = #lexer{ch = $+}) ->
    {#token{type = plus, literal = "+"}, advance(L)};
next_token(L = #lexer{ch = $=}) ->
    {#token{type = equal, literal = "="}, advance(L)};
next_token(L = #lexer{ch = $<}) ->
    {#token{type = lessthan, literal = "<"}, advance(L)};
next_token(L = #lexer{ch = $>}) ->
    {#token{type = greaterthan, literal = ">"}, advance(L)};
next_token(L = #lexer{ch = $;}) ->
    {#token{type = semicolon, literal = ";"}, advance(L)};
next_token(L = #lexer{ch = $:}) ->
    {#token{type = colon, literal = ":"}, advance(L)};
next_token(L = #lexer{ch = $(}) ->
    {#token{type = lparen, literal = "("}, advance(L)};
next_token(L = #lexer{ch = $)}) ->
    {#token{type = rparen, literal = ")"}, advance(L)};
next_token(L = #lexer{ch = ${}) ->
    {#token{type = lbrace, literal = "{"}, advance(L)};
next_token(L = #lexer{ch = $}}) ->
    {#token{type = rbrace, literal = "}"}, advance(L)};
% whitespace %
next_token(L = #lexer{ch = C}) when C == 32 orelse C >= 9 andalso C =< 13 ->
    {#token{type = whitespace}, advance(L)};
% alphabetic %
next_token(L = #lexer{ch = C})
    when C >= $a andalso C =< $z orelse C >= $A andalso C =< $Z orelse C == $_ ->
    {Ident, NewL} = read_identifier(L),
    Type =
        case Ident of
            "let" ->
                let_;
            "fn" ->
                function;
            "if" ->
                if_;
            "else" ->
                else_;
            "return" ->
                return;
            "true" ->
                boolean;
            "false" ->
                boolean;
            "bool" ->
                bool;
            "int" ->
                int;
            _ ->
                identifier
        end,
    {#token{type = Type, literal = Ident}, NewL};
% numeric %
next_token(L = #lexer{ch = C}) when C >= $0 andalso C =< $9 ->
    {Num, NewL} = read_number(L),
    {#token{type = number, literal = Num}, NewL};
% EOF %
next_token(L = #lexer{ch = eof}) ->
    {#token{type = eof}, advance(L)};
next_token(L = #lexer{ch = C}) ->
    {#token{type = illegal, literal = C}, advance(L)}.

% read_identifier %
read_identifier(L = #lexer{ch = C})
    when C >= $a andalso C =< $z orelse C >= $A andalso C =< $Z orelse C == $_ ->
    read_identifier(L, []).

read_identifier(L = #lexer{ch = C}, Acc)
    when C >= $a andalso C =< $z orelse C >= $A andalso C =< $Z orelse C == $_ ->
    NewL = advance(L),
    read_identifier(NewL, [C | Acc]);
read_identifier(L, Acc) ->
    {lists:reverse(Acc), L}.

% read_number %
read_number(L = #lexer{ch = C}) when C >= $0 andalso C =< $9 ->
    read_number(L, []).

read_number(L = #lexer{ch = C}, Acc) when C >= $0 andalso C =< $9 ->
    NewL = advance(L),
    read_number(NewL, [C | Acc]);
read_number(L, Acc) ->
    {lists:reverse(Acc), L}.

print_token(T = #token{literal = L}) ->
    case is_number(L) of
        true ->
            io:format("Token literal: \"~c\" type: ~s~n", [T#token.literal, T#token.type]);
        false ->
            io:format("Token literal: \"~s\" type: ~s~n", [T#token.literal, T#token.type])
    end.
