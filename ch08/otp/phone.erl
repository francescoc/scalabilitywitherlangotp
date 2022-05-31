%%% @copyright (c) 2013,2016 Francesco Cesarini
%%% @doc phone.erl is a mobile phone simulator which can be used to test the
%%%  phone_fsm.erl finite state machine, handling the phone state on the server side.
-module(phone).
-compile(export_all).
-define(TIMEOUT, 60000).

start_test(Num, Calls) ->
    [phone_fsm:start_link(X) || X <- lists:seq(1,Num)],
    call(Calls, Num).

call(0,_) -> ok;
call(X, Num) ->
    %%    timer:sleep(100),
    FromMs = random:uniform(Num),
    ToMs = random:uniform(Num),
    {ok, FromMsId} = hlr:lookup_id(FromMs),
    case phone_fsm:action({outbound, ToMs}, FromMsId) of
	ok ->
	    call(X-1, Num);
	_error ->
	    call(X, Num)
    end.

reply(outbound, _ToMsId, Ms) ->
    clear(),
    FromMsId = self(),
    io:format("~p dialing ~p~n",[FromMsId, _ToMsId]),
    F = fun() ->
		random:seed(now()),
		timer:sleep(random:uniform(3000)),
		io:format("~p hanging up ~p~n",[FromMsId, Ms]),
		phone_fsm:action(hangup, FromMsId)
	end,
    put(pid, spawn(F));

reply(connected, OtherMsId, _Ms) ->
    clear(),
    FromMsId = self(),
    io:format("~p connected to ~p~n",[FromMsId, OtherMsId]),
    F = fun() ->
		random:seed(now()),
		timer:sleep(random:uniform(3000)),
		io:format("~p hanging up ~p~n",[FromMsId, OtherMsId]),
		phone_fsm:action(hangup, FromMsId)
	end,
    put(pid, spawn(F));
reply(invalid, _ToMs, _Ms) ->
    io:format("~p connecting to ~p failed:invalid number~n",[_ToMs, _Ms]),
    clear();
reply(inbound, _FromMsId, _Ms) ->
    clear(),
    ToMsId = self(),
    F = fun() ->
		random:seed(now()),
		timer:sleep(random:uniform(1500)),
		case random:uniform(2) of
		    1 ->
			io:format("accept(~p,~p)~n",[ToMsId, _FromMsId]),
			phone_fsm:action(accept, ToMsId),
			timer:sleep(random:uniform(3000)),
			phone_fsm:action(hangup, ToMsId);
		    2 ->
			phone_fsm:action(reject, ToMsId)
		end
	end,
    put(pid, spawn(F));
reply(hangup, _FromMsId, _Ms) ->
    clear();
reply(_Reason, FromMsId, _Ms) ->
    io:format("~p connecting to ~p failed:~w~n",
	      [element(2,hlr:lookup_ms(FromMsId)), _Ms, _Reason]),
    clear().

clear() ->
    case get(pid) of
	undefined -> ok;
	Pid ->
	    exit(Pid, kill), erase(pid),
	    io:format("~p cleared~n",[self()])
    end.
