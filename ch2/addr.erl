-module(addr).
-export([type/1]).

-include_lib("kernel/include/inet.hrl").

type(Addr) ->
    {ok, HostEnt} = inet:gethostbyaddr(Addr),
    HostEnt#hostent.h_addrtype.
