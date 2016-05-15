%%% @copyright (c) 2013,2016 Francesco Cesarini
-module(phone_test).
-compile(export_all).

start(Num, Calls) ->
    catch frequency_sup2:start_link(),
    [phone_fsm:start_link(X) || X <- lists:seq(1,Num)],
    call(Calls, Num).

call(0,_) -> ok;
call(X, Num) ->
    MSISDN1 = random:uniform(Num),
    MSISDN2 = random:uniform(Num),
    {ok, Pid1} = hlr:lookup_id(MSISDN1),
    phone:outbound(Pid1, MSISDN2),
    timer:sleep(100),
    call(X-1, Num).
