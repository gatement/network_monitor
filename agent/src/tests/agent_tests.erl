-module(agent_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

ping_test() ->

	%% ensure the agent is started	
	case get_socket() of
		error ->
			?debugMsg("Please start the agent firstly before run the tests."),
			exit(normal);
		Socket -> 
			gen_tcp:close(Socket)
	end,
	
	%% good example	
	Password1 = get_password(),
	Cmd1 = "ping", 
	CmdData1 = {struct, [
		{"host", "localhost"}, 
		{"size", 32}, 
		{"count", 1}
	]},
	RequestData1 = get_request_data(Password1, Cmd1, CmdData1),	
	Result1 = get_response(RequestData1),
	{ok, {struct, [{"loss", Loss}, {"time", Time}]}} = json2:decode_string(lists:flatten(Result1)),	
	?assert(Loss >= 0),
	?assert(Time >= 0),
	
	
	%% wrong password	
	Password2 = "sdfsafdsfsdafe232323",
	Cmd2 = "ping", 
	CmdData2 = {struct, [
		{"host", "localhost"}, 
		{"size", 32}, 
		{"count", 1}
	]},
	RequestData2 = get_request_data(Password2, Cmd2, CmdData2),	
	Result2 = get_response(RequestData2),		
	{ok, {struct, [{"error", "wrong_password"}]}} = json2:decode_string(lists:flatten(Result2)),	
	
	%% bad command	
	Password3 = get_password(),
	Cmd3 = "bad_cmd",
	CmdData3 = {struct, [{"data", "bad_data"}]},
	RequestData3 = get_request_data(Password3, Cmd3, CmdData3),	
	Result3 = get_response(RequestData3),		
	{ok, {struct, [{"error", "not_support"}]}} = json2:decode_string(lists:flatten(Result3)),
	
	ok.
	

%% ===================================================================
%% Local Functions
%% ===================================================================

get_socket() ->
	Port = agent_config:get(agent_tcp_port),
	case gen_tcp:connect("localhost", Port, [list, {active, false}, {packet, 2}]) of
		{ok, Socket} -> Socket;
		_ -> error
	end.

	
get_password() ->
	agent_config:get(agent_password).
	
	
get_request_data(Password,Cmd, CmdData) ->
	Data = {struct, [
		{"password", Password}, 
		{"command", Cmd}, 
		{"data", CmdData}
	]},
	lists:flatten(json2:encode(Data)).
	
get_response(RequestData) ->
	Socket = get_socket(),	
    gen_tcp:send(Socket, RequestData),
    {ok, Result} = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
	Result.
	