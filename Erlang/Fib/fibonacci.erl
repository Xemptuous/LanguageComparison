-module(fibonacci).

-export([start/0]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when N > 0 ->
    fib(N - 1) + fib(N - 2).

start() ->
    [io:format("fib(~p): ~p~n", [I, fib(I)]) || I <- lists:seq(0, 30)].
