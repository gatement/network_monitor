-module(login_yaws_tests).
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% API functions
%% ===================================================================

login_yaws_test_() ->

	%% ensure the manager is started	
	case get_socket() of
		error ->
			?debugMsg("Please start the manager firstly before run the tests. Or check if the test config is good in manager.conf."),
			exit(normal);
		Socket -> 
			gen_tcp:close(Socket)
	end,

	inets:start(),


	%% normal request
	Schema = manager_config:get(test_manager_http_schema),
	Host = manager_config:get(test_manager_host),
	Port = manager_config:get(test_manager_port),
	Page = "login.yaws",

	Url1 = lists:flatten(io_lib:format("~s://~s:~p/~s", [Schema, Host, Port, Page])),
	{ok, {{"HTTP/1.1", 200, "OK"}, _, Result1}} = httpc:request(Url1),

	Query2 = "target_url=index.yaws",
	Url2 = lists:flatten(io_lib:format("~s://~s:~p/~s?~s", [Schema, Host, Port, Page, Query2])),
	{ok, {{"HTTP/1.1", 200, "OK"}, _, Result2}} = httpc:request(Url2),

	Query3 = "id=gatement",
	Url3 = lists:flatten(io_lib:format("~s://~s:~p/~s?~s", [Schema, Host, Port, Page, Query3])),
	{ok, {{"HTTP/1.1", 200, "OK"}, _, Result3}} = httpc:request(Url3),

	Query4 = "msg=wrong_password",
	Url4 = lists:flatten(io_lib:format("~s://~s:~p/~s?~s", [Schema, Host, Port, Page, Query4])),
	{ok, {{"HTTP/1.1", 200, "OK"}, _, Result4}} = httpc:request(Url4),

	Query5 = "target_url=index.yaws&id=gatement&msg=wrong_password",
	Url5 = lists:flatten(io_lib:format("~s://~s:~p/~s?~s", [Schema, Host, Port, Page, Query5])),
	{ok, {{"HTTP/1.1", 200, "OK"}, _, Result5}} = httpc:request(Url5),

	[?_assert(string:str(Result1, "Login - NetworkMonitor") > 0),
	?_assert(string:str(Result2, "rest_user/login?target_url=index.yaws") > 0),
	?_assert(string:str(Result3, "value=\"gatement\"") > 0),
	?_assert(string:str(Result4, "wrong_password") > 0),
	?_assert(string:str(Result5, "rest_user/login?target_url=index.yaws") > 0),
	?_assert(string:str(Result5, "value=\"gatement\"") > 0),
	?_assert(string:str(Result5, "wrong_password") > 0)].


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