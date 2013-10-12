-module(ping).
-export([run/3]).


run(Host, Size, Count) ->
	ping_start(Host, Size, Count).


ping_start(Host, Size, Count) ->
	PingStr = get_ping_cmd(Host, Size, Count),
	io:format("~n======== ~s ========~n", [PingStr]),

	Result = os:cmd(PingStr),
	lists:foreach(fun(Str) -> io:format("~s~n", [Str]) end, string:tokens(Result, "\r\n")),

	{LossPercentage, TimeAve} = get_result_value(Result),
	io:format("========> ~p% loss~n", [LossPercentage]),
	io:format("========> ~pms average time~n", [TimeAve]),
	io:format("(~s)~n~n", [tools:datetime_string()]),

	{LossPercentage, TimeAve}.


get_ping_cmd(Host, Size, Count) -> 
	{OS, _} = os:type(),
	case OS of
		win32 -> "ping -n " ++ erlang:integer_to_list(Count) ++ " -l " ++ erlang:integer_to_list(Size) ++ " " ++ Host;
		unix -> "ping -c " ++ erlang:integer_to_list(Count) ++ " -s " ++ erlang:integer_to_list(Size) ++ " " ++ Host
	end.


get_result_value(Result) -> 

	% Loss
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

	% Time
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
