-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/1, terminate/1, handle/2]).

%% These are the start functions used to create and
%% initialize the server.
start() -> server:start(frequency, []).

init(_Args) ->
    {get_frequencies(), []}.

%% Hard-coded
get_frequencies() -> [10,11,12,13,14,15].

stop()	         -> server:stop(frequency).
allocate()	 -> server:call(frequency, {allocate, self()}).
deallocate(Freq) -> server:call(frequency, {deallocate, Freq}).

handle({allocate, Pid}, Frequencies) ->
    allocate(Frequencies, Pid);
handle({deallocate, Freq}, Frequencies) ->
    {deallocate(Frequencies, Freq), ok}.
terminate(_Frequencies) ->
    ok.

%% The Internal Helper Functions used to allocate and
%% deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
