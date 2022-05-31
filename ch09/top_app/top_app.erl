-module(top_app).
-behaviour(application).
-export([start/2, start_phase/3, stop/1]).

start(_Type, _Args) ->
    {ok, _Pid} = bsc_sup:start_link().

start_phase(StartPhase, StartType, Args) ->
    io:format("top_app:start_phase(~p,~p,~p).~n",
              [StartPhase, StartType, Args]).

stop(_Data) ->
    ok.
