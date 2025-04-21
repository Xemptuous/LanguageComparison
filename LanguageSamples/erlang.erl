-module(showcase).

-export([run/0]).

greet(Name) ->
    io:format("Hi, ~s!~n", [Name]).

square(N) ->
    N * N.

loop(Count) when Count < 3 ->
    io:format("While loop: ~p~n", [Count]),
    loop(Count + 1);
loop(_) ->
    ok.

run() ->
    Name = "Alice",
    X = 5,
    Y = 3.14,
    Active = true,

    greet(Name),
    io:format("Square of ~p is ~p~n", [X, square(X)]),

    P = #{x => 3, y => 4},
    io:format("Point: (~p, ~p)~n", [maps:get(x, P), maps:get(y, P)]),

    Nums = [1, 2, 3],
    lists:foreach(fun(N) -> io:format("~p ", [N]) end, Nums),
    io:format("~n", []),

    Ages = #{"Alice" => 30, "Bob" => 25},
    io:format("Bob is ~p years old.~n", [maps:get("Bob", Ages)]),

    loop(0).
