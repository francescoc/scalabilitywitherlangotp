-module(ex3).
-export([filter/2, is_even/1]).

filter(P,[]) -> [];
filter(P,[X|Xs]) ->
    case P(X) of
	true ->
	    [X| filter(P,Xs)];
	_ ->
	    filter(P,Xs)
    end.

is_even(X) ->
    X rem 2 == 0.
