-module(bsc).

-behaviour(application).

%% Application callbacks
-export([start/2, start_phase/3, stop/1]).

start(_StartType, _StartArgs) ->
    bsc_sup:start_link().

start_phase(Phase, Type, Args) ->
    io:format("bsc:start_phase(~p,~p,~p).~n",[Phase, Type, Args]).

stop(_Data) ->
    ok.
