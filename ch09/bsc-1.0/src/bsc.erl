-module(bsc).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, []),
    freq_overload:add(counters, {}),
    freq_overload:add(logger, {file, "log"}),
    {ok, Pid}.

stop(_Data) ->
    ok.

%% Supervisor callbacks.

init(_) ->
    ChildSpecList = [child(freq_overload), child(frequency), child(simple_phone_sup)],
    {ok,{{rest_for_one, 2, 3600}, ChildSpecList}}.

child(Module) ->
    {Module, {Module, start_link, []},
     permanent, 2000, worker, [Module]}.
