%%% File    : frequency.erl
%%% Author  :  <francesco@erlang-consulting.com>
%%% Description : Server example from lecture notes
%%% Created : 25 Mar 2003 by  <francesco@erlang-consulting.com>

-module(timeout).
-behaviour(gen_server).

-export([init/1, handle_call/3]).

init(_Args) ->
    {ok, null}.

handle_call({sleep, Ms}, _From, LoopData) ->
    timer:sleep(Ms),
    {reply, ok, LoopData}.


