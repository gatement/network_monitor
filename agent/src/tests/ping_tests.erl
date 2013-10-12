-module(ping_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

run_test() ->
	Host = "localhost",
	Size = 32,
	Count = 1,
	{Loss, Time} = ping:run(Host, Size, Count),
	
	?assert(Loss >= 0),
	?assert(Time >= 0).
	

%% ===================================================================
%% Local Functions
%% ===================================================================



