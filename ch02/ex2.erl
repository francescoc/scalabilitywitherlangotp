-module(ex2).
-export([print_all/1,all_print/1]).

print_all([]) ->
    io:format("\n");
print_all([X|Xs]) ->
    io:format("~p\t",[X]),
    print_all(Xs).

all_print(Ys) ->
    case Ys of
	[] ->
	    io:format("~n");
	[X|Xs] ->
	    io:format("~p\t",[X]),
	    all_print(Xs)
    end.
