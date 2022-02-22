-module(earth).
-export([start/0, init/0]).

start() ->
    spawn(?MODULE, init, []).

init() ->
    create_earth(),
    day().

day() ->
    receive
	eclipse -> day();
	sunset  -> night()
    end.

night() ->
    receive
	sunrise ->
	    make_roosters_crow(),
	    day()
    end.

create_earth() -> ok.
make_roosters_crow() -> ok.
