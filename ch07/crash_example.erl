-module(crash_example).
-behaviour(gen_event).
-export([init/1, terminate/2, handle_event/2]).

init(normal) -> {ok, []};
init(return) -> error;
init(ok)     -> ok;
init(crash) -> exit(crash).

terminate(_Reason, _LoopData) -> ok.

handle_event(crash,  _LoopData) -> 1/0;
handle_event(return, _LoopData) -> error.
