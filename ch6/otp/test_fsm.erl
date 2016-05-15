-module(test_fsm).
-behaviour(gen_fsm).

-export([start_link/2, start/2]).

-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {}).

start_link(TimerMs, Options) ->
    gen_fsm:start_link(?MODULE, TimerMs, Options).
start(TimerMs, Options) ->
    gen_fsm:start(?MODULE, TimerMs, Options).

init(0) ->
    {stop, stopped};
init(1) ->
    {next_state, selection, []};
init(TimerMs) ->
    timer:sleep(TimerMs),
    ignore.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
