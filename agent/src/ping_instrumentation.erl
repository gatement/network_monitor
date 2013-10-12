-module(ping_instrumentation).

-export([ping_johnsonlau_net/1, ping_johnsonlau_net/2]).
-export([ping_163_com/1, ping_163_com/2]).


ping_johnsonlau_net(get) ->
	ping(johnsonlau_net).
	
ping_johnsonlau_net(set, _NewVal) ->
	no_implementation.


ping_163_com(get) ->
	ping('163_com').

ping_163_com(set, _NewVal) ->
	no_implementation.

%%
%% Helpers
%%

ping(Target) -> 
	{Host, Size, Count} = ping_targets:get(Target),
	Result = ping:run(Host, Size, Count),
    build_result(Result).

build_result({Loss, Time}) ->
	Result = erlang:integer_to_list(Loss) ++ "," ++ erlang:integer_to_list(Time),
	{value, Result}.
