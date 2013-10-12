-module(ping_targets).

-export([get/1]).

get(Key) ->
    {ok, [Targets]} = file:consult("../ping_targets.config"),
	proplists:get_value(Key, Targets).


