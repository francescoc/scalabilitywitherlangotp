-module(mutex0).

-export([start_link/1, init/1, stop/1]). 
-export([wait/1, signal/1]).


wait(Name) ->
    Name ! {wait,self()},
    Mutex = whereis(Name),
    receive
	    {Mutex,ok} -> ok
    end.

signal(Name) ->
    Name ! {signal,self()},
    ok.

start_link(Name) ->
    spawn_link(?MODULE, init, [Name]).

stop(Name) -> Name ! stop.

init(Name) ->
    register(Name, self()),
    process_flag(trap_exit, true),
    free(Name).


free(Name) ->
    receive
	{wait,Pid} ->		%% The user requests.
	    Pid ! {self(),ok},
	    busy(Pid, Name);
	stop ->
	    ok
    end.

busy(Pid, Name) ->
    receive
	{signal,Pid} ->
	    free(Name)
    end.
