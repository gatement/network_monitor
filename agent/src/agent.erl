-module(agent).
-export([start/0,
		server/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	Port = agent_config:get(agent_tcp_port),	
    case gen_tcp:listen(Port,[list, {active, false},{packet, 2}]) of
        {ok, ListenSocket} -> loop(ListenSocket);
        {error, Reason} -> exit(Reason)
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================
	
loop(ListenSocket) ->	
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} -> 
			%io:format("Accept socket ~p~n", [Socket]),
			spawn(?MODULE, server, [Socket]);            
        _Err -> 
			ignore
    end,
	loop(ListenSocket).

	
server(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			%io:format("Socket ~p receive:~n ~p~n", [Socket, Packet]),
			try
				{ok, {struct, [{"password", Password}, {"command", Cmd}, {"data", Data}]}} = json2:decode_string(lists:flatten(Packet)),
				Result = process({Password, Cmd, Data}),
				EncodedResult = encode_result(Result),
				%io:format("Socket ~p return:~n ~p~n", [Socket, EncodedResult]),
				gen_tcp:send(Socket, EncodedResult)
			catch
				Type:Error -> io:format("Exception: ~p(~p), request data: ~p~n", [Error, Type, Packet])
			end,
			server(Socket);
        {error, closed} ->
			%io:format("Socket ~p closed.~n", [Socket]),
            closed;
        {error, Reason} ->
			io:format("error: ~p~n", [Reason]),
            ignore
    end.
	

process({Password, Cmd, Data}) ->
	ConfigPwd = agent_config:get(agent_password),
	case Password of
		ConfigPwd ->
			case Cmd of
				"ping" -> do_ping(Data);
				_ -> 
					io:format("Command not support: ~p~n~n", [Cmd]),				
					{struct, [
						{"error", "not_support"}
					 ]}
			end;
		_ -> 
			io:format("Wrong password: ~p~n~n", [Password]),
			{struct, [
				{"error", "wrong_password"}
			]}
	end.
	
	
do_ping({struct,[{"host", Host},{"size", Size},{"count", Count}]}) ->
	{Loss, Time} = ping:run(Host, Size, Count),
	{struct, [
		{"loss", Loss}, 
		{"time", Time}
	]}.

encode_result(Result) ->
	lists:flatten(json2:encode(Result)).
	