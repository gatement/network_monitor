-module(tools).
-export([sleep/1,
		val_by_key/2, 
		get_cookie_val/2,
		random_string/1,
		generate_id/1,
		now/0,
		now_epoch_seconds_int/0,
		now_epoch_seconds_str/0,
		now_integer/0,
		now_big_integer/0,
		record_to_list/2,
		datetime_string/0,
		datetime_string2/0,
		datetime_string/6,
		date_string/0,
		date_to_string/1,
		date_add_days/2,
		date_add_days_string/2,
		universal_date/0,
		md5_str/1,
		day_of_the_week/0]).

-include("yaws_api.hrl").

%%
%% API Functions
%%

sleep(MiniSec) -> 
	receive
	after MiniSec -> ok
	end.


val_by_key(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{Key, Val} -> Val;
		false -> false
	end.


% return [] or cookie value
get_cookie_val(Arg, CookieName) ->
	yaws_api:find_cookie_val(CookieName, (Arg#arg.headers)#headers.cookie).


random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).


generate_id(Prefix) ->
	{A, B, C} = erlang:now(),
	Id = Prefix ++ integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
	Id.


now() ->
	{A, B, C} = erlang:now(),
	Now = integer_to_list(A) ++ integer_to_list(B) ++ integer_to_list(C),
	Now.
	
	
now_epoch_seconds_int() ->
	{A, B, C} = erlang:now(),
	Now = erlang:round(A*1000000 + B + C/1000000),
	Now.
	
now_epoch_seconds_str() ->
	erlang:integer_to_list(now_epoch_seconds_int()).

now_integer() ->
	{A, B, C} = erlang:now(),
	Now = erlang:round(A*1000000000 + B*1000 + C/1000), % unit: millisecond, the same as Java DateTime.getUtcMilliSecondTimestamp()
	Now.


now_big_integer() ->
	{A, B, C} = erlang:now(),
	Now = erlang:round(A*1000000000000 + B*1000000 + C), % unit: macrosecond
	Now.


record_to_list(Record, Attributes) ->
	Fun = fun(X, Acc) -> 
		[{atom_to_list(X), element(length(Acc) + 2, Record)} | Acc] 
	end,
	lists:foldl(Fun, [], Attributes).


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



datetime_string2() ->
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

	erlang:integer_to_list(Year) ++ MonthText ++ DayText ++ "_" ++ HourText ++ MinuteText ++ SecondText.


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


date_to_string({Year, Month, Day}) ->
	MonthText = if
		Month < 10 -> "0" ++ erlang:integer_to_list(Month);
		true -> erlang:integer_to_list(Month)
	end,
	DayText = if
		Day < 10 -> "0" ++ erlang:integer_to_list(Day);
		true -> erlang:integer_to_list(Day)
	end,
	erlang:integer_to_list(Year) ++ "-" ++ MonthText ++ "-" ++ DayText.


%% Date is {Year, Month, Day}
date_add_days(Date, Days) ->
	calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Days).


%% Date is {Year, Month, Day}
date_add_days_string(Date, Days) ->
	{Year2, Month2, Day2} = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Days),
	date_to_string({Year2, Month2, Day2}).


universal_date() ->
	{Date, _} = calendar:universal_time(),
	Date.

	
md5_str(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = erlang:binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

	
day_of_the_week() ->
	{Year, Month, Day} = erlang:date(),
	calendar:day_of_the_week(Year, Month, Day).

%%
%% Local Functions
%%
    
list_to_hex(L) ->
    lists:map(fun(X) -> ini_to_hex(X) end, L).
    
ini_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

    
hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).


