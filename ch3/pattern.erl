%%% @author Francesco Cesarini <francescoc@ramone>
%%% @copyright (C) 2015, Francesco Cesarini
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2015 by Francesco Cesarini <francescoc@ramone>

-module(pattern).
-export([start/1, init/1]).

start(Args) ->                      % Start the server.
    spawn(server, init, [Args]).

init(Args) ->                       % Initialize the internal process state
    State = initialize_state(Args),
    loop(State).

loop(State) ->                      % Receive and handle messages.
    receive
	{handle, Msg} ->
	    NewState = handle(Msg, State),
	    loop(NewState);
	stop -> 
	    terminate(State)      % Stop the process.
    end.

terminate(State) ->                % Cleanup prior to termination.
    clean_up(State).

%% TODO: Fill in specific server code
handle(_, _) -> ok.
initialize_state(_) -> ok.
clean_up(_) -> ok.
