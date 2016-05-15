-module(phone_sup).
-behaviour(supervisor).

-export([start_link/0, attach_phone/1, detach_phone/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 3600}, []}}.


%%Race condition risk. Device attached after we check but before we start

attach_phone(Ms) ->
    case hlr:lookup_id(Ms) of
	{ok, _Pid}    ->
	    {error, attached};
	_NotAttached ->
	    ChildSpec = {Ms, {phone_fsm, start_link, [Ms]},
			 transient, 2000, worker, [phone_fsm]},
	    supervisor:start_child(?MODULE, ChildSpec)
    end.

detach_phone(Ms) ->
    case hlr:lookup_id(Ms) of
	{ok, _Pid}    ->
	    supervisor:terminate_child(?MODULE, Ms),
	    supervisor:delete_child(?MODULE, Ms);
	_NotAttached ->
	    {error, detached}
    end.
