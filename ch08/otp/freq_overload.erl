-module(freq_overload).
-export([start_link/0, add/2, delete/2]).
-export([no_frequency/0, frequency_available/0, frequency_denied/0]).

start_link() -> gen_event:start_link({local, ?MODULE}).

no_frequency() -> gen_event:notify(?MODULE, {set_alarm, {no_frequency, self()}}).
frequency_available() -> gen_event:notify(?MODULE, {clear_alarm, no_frequency}).
frequency_denied() -> gen_event:notify(?MODULE, {event, {frequency_denied, self()}}).

add(M,A) -> gen_event:add_sup_handler(?MODULE, M, A).
delete(M,A) -> gen_event:delete_handler(?MODULE,M,A).
