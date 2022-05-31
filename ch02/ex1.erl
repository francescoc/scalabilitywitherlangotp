-module(ex1).
-export([factorial/1]).

factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N-1).

%% factorial(_) ->
%%     {error,bad_argument}.
