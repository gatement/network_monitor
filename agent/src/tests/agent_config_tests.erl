-module(agent_config_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

get_test_() ->
	[
	 	?_assert(erlang:length(agent_config:get(agent_password)) > 0),
	 	?_assert(agent_config:get(agent_tcp_port) > 0),
	 	?_assert(agent_config:get(agent_tcp_port) < 65536)
	].


%% ===================================================================
%% Local Functions
%% ===================================================================


