-module(tools).
-export([sleep/1,
		 list_string_to_string/3,
		 epoch_seconds/0,
		datetime_string/1,
		datetime_string/2,
		get_date/1,
		get_date/2,
		get_datetime/1,
		get_datetime/2]).

%% ===================================================================
%% API functions
%% ===================================================================

sleep(Milliseconds) -> 
	receive
	after Milliseconds -> ok
	end.


list_string_to_string([], _, Acc) ->
	lists:flatten(Acc);
list_string_to_string([H|T], Separator, []) ->
	list_string_to_string(T, Separator, H);
list_string_to_string([H|T], Separator, Acc) ->
	list_string_to_string(T, Separator, [Acc ++ Separator ++ H]). 


epoch_seconds() ->
	{A, B, C} = erlang:now(),
	erlang:round(A*1000000 + B + C/1000000).


datetime_string(Format) ->
	Date = erlang:date(),
	Time = erlang:time(),
	datetime_string(Format, {Date, Time}).


datetime_string(Format, {{Year, Month, Day}, {Hour, Minute, Second}}) ->
	MonthText = if
		Month < 10 -> "0" ++ erlang:integer_to_list(Month);
		true -> erlang:integer_to_list(Month)
	end,
	DayText = if
		Day < 10 -> "0" ++ erlang:integer_to_list(Day);
		true -> erlang:integer_to_list(Day)
	end,
	HourText = if
		Hour < 10 -> "0" ++ erlang:integer_to_list(Hour);
		true -> erlang:integer_to_list(Hour)
	end,
	MinuteText = if
		Minute < 10 -> "0" ++ erlang:integer_to_list(Minute);
		true -> erlang:integer_to_list(Minute)
	end,
	SecondText = if
		Second < 10 -> "0" ++ erlang:integer_to_list(Second);
		true -> erlang:integer_to_list(Second)
	end,

	Result = case Format of
		'yyyyMMdd hh:mm' ->
			erlang:integer_to_list(Year) ++ MonthText ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText;
		'yyyyMMdd hh:mm:ss' ->
			erlang:integer_to_list(Year) ++ MonthText ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText ++ ":" ++ SecondText;
		'yyyy-MM-dd hh:mm:ss' ->
			erlang:integer_to_list(Year) ++ "-" ++ MonthText ++ "-" ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText ++ ":" ++ SecondText
	end,

	lists:flatten(Result).
	

get_date(Days) ->
	Date = erlang:date(),
	get_date(Days, Date).


get_date(Days, Date) ->
	GregorianDays = calendar:date_to_gregorian_days(Date),
	calendar:gregorian_days_to_date(GregorianDays + Days).
		

get_datetime(Seconds) ->
	Date = erlang:date(),
	Time = erlang:time(),
	get_datetime(Seconds, {Date, Time}).


get_datetime(Seconds, Datetime) ->
	GregorianSeconds = calendar:datetime_to_gregorian_seconds(Datetime),
	calendar:gregorian_seconds_to_datetime(GregorianSeconds + Seconds).
		

%% ===================================================================
%% Local Functions
%% ===================================================================

