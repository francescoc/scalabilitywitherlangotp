-module(coffee_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-vsn('1.0').

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    coffee_sup:start_link().

stop(_State) ->
    ok.
