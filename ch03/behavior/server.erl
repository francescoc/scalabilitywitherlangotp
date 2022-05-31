-module(server).
-export([start/2, stop/1, call/2]).
-export([init/2]).

start(Name, Args) ->
    register(Name, spawn(server, init, [Name, Args])).

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

init(Mod, Args) ->
    State = Mod:init(Args),
    loop(Mod, State).


call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive {reply, Reply} -> Reply end.

reply(To, Reply) ->
    To ! {reply, Reply}.

loop(Mod, State) ->
    receive
	{request, From, Msg} ->
	    {NewState, Reply} = Mod:handle(Msg, State),
	    reply(From, Reply),
	    loop(Mod, NewState);
	{stop, From}  ->
	    Reply = Mod:terminate(State),
	    reply(From, Reply)
    end.
