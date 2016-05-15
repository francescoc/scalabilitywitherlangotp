%%%-------------------------------------------------------------------
%%% @author Francesco Cesarini <francescocesarini@macbook-pro-2.local>
%%% @copyright (C) 2013, Francesco Cesarini
%%% @doc
%%%
%%% @end
%%% Created :  8 Jun 2013 by Francesco Cesarini <francescocesarini@macbook-pro-2.local>
%%%-------------------------------------------------------------------
-module(phone_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, terminate/3, handle_sync_event/4]).
%% Client Functions
-export([inbound/1, action/2, busy/1, reject/1, accept/1, hangup/1]).
%% States
-export([idle/2, calling/2, connected/2, receiving/2]).


start_link(Ms) ->
    gen_fsm:start_link(?MODULE, Ms, [{debug, [trace]}]).
%%    gen_fsm:start_link(?MODULE, Ms, []).

init(Ms) ->
    process_flag(trap_exit, true),
    hlr:attach(Ms),
    {ok, idle, Ms}.


%% Events from the Phone
action({outbound, ToMs}, MsId) ->
    gen_fsm:sync_send_all_state_event(MsId, {outbound, ToMs});
action(Action, MsId) ->  %Action == hangup, reject, accept
    
    gen_fsm:send_event(MsId, {action,Action}).


busy(ToMsId) ->
    gen_fsm:send_event(ToMsId, {busy, self()}).
reject(ToMsId) ->
    gen_fsm:send_event(ToMsId, {reject, self()}).
accept(ToMsId) ->
    gen_fsm:send_event(ToMsId, {accept, self()}).
hangup(ToMsId) ->
    gen_fsm:send_event(ToMsId, {hangup, self()}).
inbound(ToMsId) ->
    gen_fsm:send_event(ToMsId, {inbound, self()}).
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

%% Event outbound in state idle is synchronous and handled in 
%% handle_sync_event

idle({inbound, FromMsId}, Ms) ->
    phone:reply(inbound, FromMsId, Ms),
    {next_state, receiving, {Ms, FromMsId}};
idle(_Ignore, State) ->  % , hangup, reject, accept
    io:format("~p in idle, ignored. State:~w, Event:~w~n",[self(), State, _Ignore]),
    {next_state, idle, State}.


%% Beware of race conditions. This event could be received right after
%% the other party accepts, meaning we would handle it not here but in state
%% connected. 

calling({action, hangup}, {Ms, CallingMsId}) ->
    phone_fsm:hangup(CallingMsId),
    {next_state, idle, Ms};


calling({busy, Pid}, {Ms, Pid}) ->
    phone:reply(busy, Pid, Ms),
    {next_state, idle, Ms};
calling({reject, Pid}, {Ms, Pid}) ->
    phone:reply(rejected, Pid, Ms),
    {next_state, idle, Ms};
calling({accept, Pid}, {Ms, Pid}) ->
    case frequency:allocate() of
	{error, no_frequency} ->
	    phone_fsm:reject(Pid),
	    phone:reply(no_frequency, Pid, Ms),
	    {next_state, idle, Ms};
	{ok, Freq} ->
	    phone:reply(connected, Pid, Ms),
	    {next_state, connected, {Ms, Pid, Freq}}
    end;
calling({inbound, Pid}, State) ->
    phone_fsm:busy(Pid),
    {next_state, calling, State};
calling(_Ignore, State) ->  % {action, reject}, {action, accept}
    io:format("In calling, ignored. State:~w, Event:~w~n",[State, _Ignore]),
    {next_state, calling, State}.

connected({inbound, FromMsId}, State) ->
    phone_fsm:busy(FromMsId),
    {next_state, connected, State};
connected({action, hangup}, {Ms, OtherMsId, Freq}) -> %% We hang up, We initated call
    phone_fsm:hangup(OtherMsId),
    frequency:deallocate(Freq),
    {next_state, idle, Ms};
connected({action, hangup}, {Ms, OtherMsId}) -> %% We hang up, Other initated call
    phone_fsm:hangup(OtherMsId),
    {next_state, idle, Ms};

connected({hangup, OtherMsId}, {Ms, OtherMsId}) -> %% they hang Up
    phone:reply(hangup, OtherMsId, Ms), 
    {next_state, idle, Ms};
connected({hangup, OtherMsId}, {Ms, OtherMsId, Freq}) -> %% they hang Up
    phone:reply(hangup, OtherMsId, Ms), 
    frequency:deallocate(Freq),
    {next_state, idle, Ms};
connected(_Ignore, State) ->
    io:format("In connected, ignored. State:~w, Event:~w~n",[State, _Ignore]),
    {next_state, connected, State}.

receiving({action, accept}, {Ms, FromMsId}) ->
    phone_fsm:accept(FromMsId),
    {next_state, connected, {Ms, FromMsId}};
receiving({action, reject}, {Ms, FromMsId}) ->
    phone_fsm:reject(FromMsId),
    {next_state, idle, Ms};
receiving({hangup, FromMsId}, {Ms, FromMsId}) ->
    phone:reply(hangup, FromMsId, Ms),
    {next_state, idle, Ms};
receiving({inbound, FromMsId}, State) ->  %Others
    phone_fsm:busy(FromMsId),
    {next_state, receiving, State};
receiving(_Ignore, State) ->  % {action, hangup}
  io:format("In receiving, ignored. State:~w, Event:~w~n",[State, _Ignore]),
    {next_state, receiving, State}.

handle_sync_event({outbound, ToMs}, _From, idle, Ms) ->
    case hlr:lookup_id(ToMs) of
	{error, invalid} ->
	    io:format("ERROR, INVALID~n"),
	    phone:reply(invalid, ToMs, Ms),
	    {reply, {error, invalid}, idle, Ms};
	{ok, ToMsId} when is_pid(ToMsId) ->
	    phone:reply(outbound, ToMs, Ms),
	    phone_fsm:inbound(ToMsId),
	    {reply, ok, calling, {Ms, ToMsId}}
    end;
handle_sync_event({outbound, _ToMSISDN}, _From, State, MSISDN) ->
    {reply, {error, busy}, State, MSISDN}.

