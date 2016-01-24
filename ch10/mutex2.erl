-module(mutex2).

-export([start_link/1, start_link/2, init/3, stop/1]).

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
    start_link(Name, []).

start_link(Name, DbgOpts) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, DbgOpts]).

stop(Name) -> Name ! stop.

init(Parent, Name, DbgOpts) ->
    register(Name, self()),
    process_flag(trap_exit, true),
    Debug = sys:debug_options(DbgOpts),
    proc_lib:init_ack({ok,self()}),
    free(Name, Parent, Debug).

free(Name, Parent, Debug) ->
    receive
	{wait,Pid} ->
	    link(Pid),
	    Pid ! {self(),ok},
	    busy(Pid, Name, Parent, Debug);
	stop ->
	    terminate(shutdown, Name);
	{'EXIT',Parent,Reason} ->
	    terminate(Reason, Name)
    end.

busy(Pid, Name, Parent, Debug) ->
    receive
	{signal,Pid} ->
	    free(Name, Parent, Debug);
	{'EXIT',Parent,Reason} ->
	    exit(Pid, Reason),
	    terminate(Reason, Name)
    end.

terminate(Reason, Name) ->
    unregister(Name),
    terminate(Reason).
terminate(Reason) ->
    receive
	{wait,Pid} ->
	    exit(Pid, Reason),
	    terminate(Reason)
    after 0 ->
	    exit(Reason)
    end.
