-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.
start() -> register(frequency, spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

%% Hard-coded
get_frequencies() -> [10,11,12,13,14,15].

stop()	         -> call(stop).
allocate()	 -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

call(Message) ->
    frequency ! {request, self(), Message},
    receive
	{reply, Reply} -> Reply
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

loop(Frequencies) ->
    receive
	{request, Pid, allocate} ->
	    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
	    reply(Pid, Reply),
	    loop(NewFrequencies);
	{request, Pid , {deallocate, Freq}} ->
	    NewFrequencies = deallocate(Frequencies, Freq),
	    reply(Pid, ok),
	    loop(NewFrequencies);
	{request, Pid, stop} ->
	    reply(Pid, ok)
    end.


%% The Internal Helper Functions used to allocate and
%% deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
