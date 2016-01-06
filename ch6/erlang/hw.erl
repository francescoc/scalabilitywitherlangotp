%%% File    : hardware.erl
%%% Author  :  <francesco@erlang-consulting.com>
%%% Description : Coffee Automoata stub
%%% Created : 21 Mar 2004 by  <francesco@erlang-consulting.com>

-module(hw).
-compile(export_all).

display(Str, Arg)      -> io:format("Display:" ++ Str ++ "~n", Arg).
return_change(Payment) -> io:format("Machine:Returned ~w in change~n",[Payment]).
drop_cup()             -> io:format("Machine:Dropped Cup.~n").
prepare(Type)          -> io:format("Machine:Preparing ~p.~n",[Type]).
reboot()               -> io:format("Machine:Rebooted Hardware~n").

