-module(arithmetic).

-export([add/2, divide/2, multiply/2]).

add(X, Y) ->
    X + Y.

divide(X, Y) ->
    X / Y.

multiply(X, Y) ->
    % intentionally naive multiply
    Result = lists:foldl(fun(_, Seq) ->
                                      X + Seq
                              end, 0, lists:seq(1, Y)),
    Result.
