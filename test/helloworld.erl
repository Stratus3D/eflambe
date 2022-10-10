-module(helloworld).

-export([greet/1]).

greet(Name) ->
    io:format("Hello ~p~n", [Name]).
