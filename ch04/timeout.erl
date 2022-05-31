-module(timeout).
-behaviour(gen_server).

-export([init/1, handle_call/3]).

init(_Args) ->
    {ok, undefined}.

handle_call({sleep, Ms}, _From, LoopData) ->
    timer:sleep(Ms),
    {reply, ok, LoopData}.
