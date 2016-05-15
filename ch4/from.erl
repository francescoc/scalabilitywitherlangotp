-module(from).
-behaviour(gen_server).
-export([init/1, handle_call/3]).

init(Sum) ->
    {ok, Sum}.

handle_call({add, Data}, From, Sum) ->
    gen_server:reply(From, ok),
    timer:sleep(1000),
    NewSum = add(Data, Sum),
    io:format("From:~p, Sum:~p~n",[From, NewSum]),
    {noreply, NewSum}.

add(Data, Sum) ->
    Data + Sum.
