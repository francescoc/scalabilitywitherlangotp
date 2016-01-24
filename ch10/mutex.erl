-module(mutex).

-export([start_link/1, start_link/2, init/3, stop/1]).
-export([wait/1, signal/1]).
-export([system_continue/3, system_terminate/4]).

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
    NewDebug = sys:handle_debug(Debug, fun debug/3, Name, init),
    free(Name, Parent, NewDebug).


free(Name, Parent, Debug) ->
    receive
	{wait,Pid} ->		%% The user requests.
	    NewDebug = sys:handle_debug(Debug, fun debug/3, Name, {wait,Pid}),
	    Pid ! {self(),ok},
	    busy(Pid, Name, Parent, NewDebug);
	{system,From,Msg} ->	%% The system messages.
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, {free, Name});
	stop ->
	    terminate(stopped, Name, Debug);
	{'EXIT',Parent,Reason} ->
	    terminate(Reason, Name, Debug)
    end.

busy(Pid, Name, Parent, Debug) ->
    receive
	{signal,Pid} ->
	    NewDebug = sys:handle_debug(Debug, fun debug/3, Name, {signal,Pid}),
	    free(Name, Parent, NewDebug);
	{system,From,Msg} ->   	%% The system messages.
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, {busy,Name,Pid});
	{'EXIT',Parent,Reason} ->
	    exit(Pid, Reason),
	    terminate(Reason, Name, Debug)
    end.

debug(Dev, Event, Name) ->
    io:format(Dev, "mutex ~w: ~w~n", [Name,Event]).

system_continue(Parent, Debug, {busy,Name,Pid}) ->
    busy(Pid, Name, Parent, Debug);
system_continue(Parent, Debug, {free,Name}) ->
    free(Name, Parent, Debug).

system_terminate(Reason, _Parent, Debug, {busy,Name,Pid}) ->
    exit(Pid, Reason),
    terminate(Reason, Name, Debug);
system_terminate(Reason, _Parent, Debug, {free,Name}) ->
    terminate(Reason, Name, Debug).

terminate(Reason, Name, Debug) ->
    unregister(Name),
    sys:handle_debug(Debug, fun debug/3, Name, {terminate, Reason}),
    terminate(Reason).
terminate(Reason) ->
    receive
	  {wait,Pid} ->
	    exit(Pid, Reason),
	    terminate(Reason)
    after 0 ->
	    exit(Reason)
    end.
