-module(ping).
-compile(export_all).


run(Host, Size, Count, OS) ->
	case OS of
		"windows" -> ping_start(Host, Size, Count, OS);
		"unix" -> ping_start(Host, Size, Count, OS);
		_ ->
			io:format("doesn't support OS: ~p.~n~n", [OS]),
			{100, 0}
	end.


ping_start(Host, Size, Count, OS) ->
	PingStr = get_ping_cmd(Host, Size, Count, OS),
	io:format("~n======== ~s ========~n", [PingStr]),

	Result = os:cmd(PingStr),
	lists:foreach(fun(Str) -> io:format("~s~n", [Str]) end, string:tokens(Result, "\r\n")),

	{LossPercentage, TimeAve} = get_result_value(Result),
	io:format("========> ~p% loss~n", [LossPercentage]),
	io:format("========> ~pms average time~n", [TimeAve]),
	io:format("(~s)~n~n", [tools:datetime_string()]),

	{LossPercentage, TimeAve}.


get_ping_cmd(Host, Size, Count, OS) -> 
	case OS of
		"windows" -> "ping -n " ++ erlang:integer_to_list(Count) ++ " -l " ++ erlang:integer_to_list(Size) ++ " " ++ Host;
		"unix" -> "ping -c " ++ erlang:integer_to_list(Count) ++ " -s " ++ erlang:integer_to_list(Size) ++ " " ++ Host
	end.


get_result_value(Result) -> 
	case re:run(Result, "\\(\\d+% loss") of
		{match,[{Start, Len}]} -> Loss = erlang:list_to_integer(string:substr(Result, Start + 2, Len - 7));

		_ -> case re:run(Result, " \\d+% packet loss") of
				{match,[{Start, Len}]} -> Loss = erlang:list_to_integer(string:substr(Result, Start + 2, Len - 14));

				_ -> case re:run(Result, " [\\d\\.]+% packet loss") of
						{match,[{Start, Len}]} -> Loss = erlang:round(erlang:list_to_float(string:substr(Result, Start + 2, Len - 14)));

						_ -> Loss = 100
					end
			end
	end,

	case re:run(Result, "Average = \\d+ms") of
		{match,[{Start2, Len2}]} ->	Time = erlang:list_to_integer(string:substr(Result, Start2 + 11, Len2 - 12));

		_ -> case re:run(Result, "min/avg/max/mdev = [\\d,\\.]+/[\\d,\\.]+/[\\d,\\.]+/[\\d,\\.]+ ms") of
				{match,[{Start2, Len2}]} ->	
					SubStr = string:substr(Result, Start2 + 1, Len2),
					AveTimeStr = lists:nth(5, string:tokens(SubStr, "/")),
					Time = erlang:round(erlang:list_to_float(AveTimeStr));

				_ -> case re:run(Result, "min/avg/max/stddev = [\\d,\\.]+/[\\d,\\.]+/[\\d,\\.]+/[\\d,\\.]+ ms") of
						{match,[{Start2, Len2}]} ->	
							SubStr = string:substr(Result, Start2 + 1, Len2),
							AveTimeStr = lists:nth(5, string:tokens(SubStr, "/")),
							Time = erlang:round(erlang:list_to_float(AveTimeStr));

						_ -> Time = 0
					end
			end
	end,

	{Loss, Time}.
