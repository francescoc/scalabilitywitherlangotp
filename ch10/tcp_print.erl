-module(tcp_print).
-export([init_request/0, get_request/2, stop_request/2]).
-behaviour(tcp_wrapper).


init_request() ->
    io:format("Receiving Data~n."), 
    {ok,[]}.
get_request(Data, Buffer)->
    io:format("."),
    {ok, [Data|Buffer]}.
stop_request(_Reason, Buffer) ->
    io:format("~n"),
    io:format(lists:reverse(Buffer)),
    io:format("~n").

