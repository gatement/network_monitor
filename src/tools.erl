%% Author: johnson
%% Created: Jun 22, 2012
%% Description: tools helper methods
-module(tools).

%%
%% Include files
%%


%%
%% Exported Functions
%%

-export([sleep/1,
		now_integer/0,
		datetime_string/0,
		datetime_string/6,
		date_string/0,
		day_of_the_week/0]).


%%
%% API Functions
%%

sleep(MiniSec) -> 
	receive
	after MiniSec -> ok
	end.


now_integer() ->
	{A, B, C} = erlang:now(),
	Now = erlang:round(A*1000000000 + B*1000 + C/1000), % unit: millisecond, the same as Java DateTime.getUtcMilliSecondTimestamp()
	Now.


datetime_string() ->
	{Year, Month, Day} = erlang:date(),
	{Hour, Minute, Second} = erlang:time(),

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

	erlang:integer_to_list(Year) ++ "-" ++ MonthText ++ "-" ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText ++ ":" ++ SecondText.



datetime_string(Year, Month, Day, Hour, Minute, Second) ->
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

	erlang:integer_to_list(Year) ++ "-" ++ MonthText ++ "-" ++ DayText ++ " " ++ HourText ++ ":" ++ MinuteText ++ ":" ++ SecondText.


date_string() ->
	{Year, Month, Day} = erlang:date(),

	MonthText = if
		Month < 10 -> "0" ++ erlang:integer_to_list(Month);
		true -> erlang:integer_to_list(Month)
	end,
	DayText = if
		Day < 10 -> "0" ++ erlang:integer_to_list(Day);
		true -> erlang:integer_to_list(Day)
	end,

	erlang:integer_to_list(Year) ++ "-" ++ MonthText ++ "-" ++ DayText.


day_of_the_week() ->
	{Year, Month, Day} = erlang:date(),
	calendar:day_of_the_week(Year, Month, Day).


%% Local Functions
%%

