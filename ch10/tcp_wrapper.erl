-module(tcp_wrapper).
-export([start_link/2, cast/3]).
-export([init/3, system_continue/3, system_terminate/4, init_request/2]).
%-export([behaviour_info/1]).

-callback init_request() -> {'ok', Reply :: term()}.
-callback get_request(Data :: term(), LoopData :: term()) -> {'ok', Reply :: term()} | 
							     {'stop', Reason :: atom(), LoopData :: term()}.
-callback stop_request(Reason :: term(), LoopData :: term()) -> term().

%behaviour_info(callbacks) ->
%    [{init_request, 0},{get_request, 2},{stop_request, 2}].

start_link(Mod, Port) ->
    proc_lib:start_link(?MODULE, init, [Mod, Port, self()]).

cast(Host, Port, Data) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}, {reuseaddr, true}]),
    send(Socket, Data),
    ok = gen_tcp:close(Socket).

send(Socket, <<Chunk:1/binary,Rest/binary>>) ->
    gen_tcp:send(Socket, [Chunk]),
    send(Socket, Rest);
send(Socket, <<Rest/binary>>) ->
    gen_tcp:send(Socket, Rest).

init(Mod, Port, Parent) ->
    {ok, Listener} = gen_tcp:listen(Port, [{active, false}]),
    proc_lib:init_ack({ok, self()}),
    loop(Mod, Listener, Parent, sys:debug_options([])).

loop(Mod, Listener, Parent, Debug) ->
    receive
	{system,From,Msg} ->
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, {Listener, Mod});
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Listener, Debug);
	{'EXIT', Child, _Reason} ->
	    NewDebug = sys:handle_debug(Debug, fun debug/3, stop_request, Child),
	    loop(Mod, Listener, Parent, NewDebug)
    after 0 ->
	    accept(Mod, Listener, Parent, Debug)
    end.

accept(Mod, Listener, Parent, Debug) ->
    case gen_tcp:accept(Listener, 1000) of
	{ok, Socket} ->
	    Pid = proc_lib:spawn_link(?MODULE, init_request, [Mod, Socket]),
	    gen_tcp:controlling_process(Socket, Pid),
	    NewDebug = sys:handle_debug(Debug, fun debug/3, init_request, Pid),
	    loop(Mod, Listener, Parent, NewDebug);
	{error, timeout} ->
	    loop(Mod, Listener, Parent, Debug);
	{error, Reason} ->
	    NewDebug = sys:handle_debug(Debug, fun debug/3, error, Reason),
	    terminate(Reason, Listener, NewDebug)
    end.

system_continue(Parent, Debug, {Listener, Mod}) ->
    loop(Mod, Listener, Parent, Debug).

system_terminate(Reason, _Parent, Debug, {Listener, _Mod}) ->
    terminate(Reason, Listener, Debug).

terminate(Reason, Listener, Debug) ->
    sys:handle_debug(Debug, fun debug/3, terminating, Reason),
    gen_tcp:close(Listener),
    exit(Reason).

debug(Dev, Event, Data) ->
    io:format(Dev, "Listener ~w:~w~n", [Event,Data]).

init_request(Mod, Socket) ->
    {ok, LoopData} = Mod:init_request(),
    get_request(Mod, Socket, LoopData).

get_request(Mod, Socket, LoopData) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    case Mod:get_request(Data, LoopData) of
		{ok, NewLoopData} ->
		    get_request(Mod, Socket, NewLoopData);
		{stop, Reason, NewLoopData} ->
		    gen_tcp:close(Socket),
		    stop_request(Mod, Reason, NewLoopData)
	    end;
	{error, Reason} ->
	    stop_request(Mod, Reason, LoopData)
    end.

stop_request(Mod, Reason, LoopData) ->
    Mod:stop_request(Reason, LoopData).
