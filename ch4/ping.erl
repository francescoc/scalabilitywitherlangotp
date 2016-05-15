-module(ping).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_info/2]).
-define(TIMEOUT, 5000).

init(_Args) ->
    {ok, undefined, ?TIMEOUT}.

handle_call(start, _From, LoopData) ->
    {reply, started, LoopData, ?TIMEOUT};
handle_call(pause, _From, LoopData) ->
    {reply, paused, LoopData}.

handle_info(timeout, LoopData) ->
    {_Hour,_Min,Sec} = time(),
    io:format("~2.w~n",[Sec]),
    {noreply, LoopData, ?TIMEOUT}.
