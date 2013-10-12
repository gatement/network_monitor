-module(rest_user_tests).
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

login_test_() ->
	%% ensure the manager is started	
	case get_socket() of
		error ->
			?debugMsg("Please start the manager firstly before run the tests. Or check if the test config is good in manager.conf."),
			exit(normal);
		Socket -> 
			gen_tcp:close(Socket)
	end,

	inets:start(),

	Schema = manager_config:get(test_manager_http_schema),
	Host = manager_config:get(test_manager_host),
	Port = manager_config:get(test_manager_port),
	Page = "rest_user/login",
	Query = "target_url=/index.yaws",

	Url = lists:flatten(io_lib:format("~s://~s:~p/~s?~s", [Schema, Host, Port, Page, Query])),
	Body = "id=johnson&pwd=123456",
	{ok, {{"HTTP/1.1", StatusCode, Reason}, Headers, _}} = httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Body}, [], []),
	Location = yaws_api:url_decode(proplists:get_value("location", Headers)),

	?debugVal(Headers),


	[
		?_assert(StatusCode =:= 302)
		,?_assert(Reason =:= "Found")
		,?_assert(string:str(Location, "Wrong username/password.")>0)
	].


logout_test() ->
	ok.


logout_noredirect_test() ->
	ok.


not_found_test() ->
	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================


get_socket() ->
	Host = manager_config:get(test_manager_host),
	Port = manager_config:get(test_manager_port),

	case gen_tcp:connect(Host, Port, [list, {active, false}, {packet, 0}]) of
		{ok, Socket} -> Socket;
		_ -> error
	end.