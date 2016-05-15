-module(bsc).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, start_phase/3, stop/1, init/1]).

start(_Type, _Args) ->
    {ok, Pid} = supervisor:start_link({local,?MODULE},?MODULE, []).

start_phase(Phase, Type, Args) ->
    io:format("bsc:start_phase(~p,~p,~p).",[Phase, Type, Args]).

stop(_Data) ->
    ok.

%% Supervisor callbacks.

init(_) ->
    ChildSpecList = [child(freq_overload),
                     child(frequency),
                     child(simple_phone_sup)],
    {ok,{{rest_for_one, 2, 3600}, ChildSpecList}}.

child(Module) ->
    {Module, {Module, start_link, []},
     permanent, 2000, worker, [Module]}.
