-module(echo_link).
-export([go/0, loop/0]).

go() ->
    Pid = spawn_link(echo, loop, []),
    Pid ! {self(), hello},
    receive
	{Pid, Msg} ->
	    io:format("~w~n",[Msg])
    end,
    Pid ! stop.

loop() ->
    receive
	{From, Msg} ->
	    From ! {self(), Msg},
	    loop();
	stop ->
	    ok
    end.
