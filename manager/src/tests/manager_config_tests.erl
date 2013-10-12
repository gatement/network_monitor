-module(manager_config_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

get_test_() ->
	[
	 	?_assert(erlang:length(manager_config:get(rrdtool_exe)) > 0),
	 	?_assert(erlang:length(manager_config:get(rrdtool_db)) > 0),
	 	?_assert(erlang:length(manager_config:get(rrdtool_img_abs)) > 0),
	 	?_assert(erlang:length(manager_config:get(rrdtool_img_rel)) > 0)
	].


%% ===================================================================
%% Local Functions
%% ===================================================================


