-module(tools_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

sleep_test_() ->
	erlang:statistics(wall_clock),
	tools:sleep(100),
	{_, TimeElapse} = erlang:statistics(wall_clock),
	[?_assert(TimeElapse >= 100)].	


list_string_to_string_test_() ->
	List = ["aa", "bb", "cc"],
	Expected = "aa,bb,cc",
	Actual = tools:list_string_to_string(List, ",", []),
	[?_assertEqual(Expected, Actual)].


epoch_seconds_test_() ->
	Now1 = tools:epoch_seconds(),
	Now2 = tools:epoch_seconds(),
	[?_assert(Now1 > 0),
	 ?_assert(Now2 > 0),
	 ?_assert(Now1 =< Now2)].


datetime_string_test_() ->
	[?_assertEqual(erlang:length(tools:datetime_string('yyyyMMdd hh:mm')), 14),
	?_assertEqual(erlang:length(tools:datetime_string('yyyyMMdd hh:mm:ss')), 17),
	?_assertEqual(erlang:length(tools:datetime_string('yyyy-MM-dd hh:mm:ss')), 19),
	?_assertEqual(tools:datetime_string('yyyyMMdd hh:mm', {{2012, 2, 31}, {11, 25, 0}}), "20120231 11:25"),
	?_assertEqual(tools:datetime_string('yyyyMMdd hh:mm:ss', {{2012, 3, 31}, {11, 25, 43}}), "20120331 11:25:43"),
	?_assertEqual(tools:datetime_string('yyyy-MM-dd hh:mm:ss', {{2012, 10, 31}, {11, 25, 43}}), "2012-10-31 11:25:43")].


get_date_test_() ->
	Date11 = tools:get_date(-2),
	{Year, Month, Day} = Date11,
	Date12 = tools:get_date(0),
	
	Date21 = tools:get_date(2, {2012, 10, 31}),
	Date22 = tools:get_date(-2, {2012, 10, 31}),
	
	[?_assert(Year > 0),
	?_assert(Month > 0),
	?_assert(Day > 0),
	?_assert(Date11 < Date12),
	?_assertEqual(Date21, {2012, 11, 2}),
	?_assertEqual(Date22, {2012, 10, 29})].


get_datetime_test_() ->
	Datetime11 = tools:get_datetime(-10),
	{{Year, Month, Day}, {Hour, Minute, Second}} = Datetime11,
	Datetime12 = tools:get_datetime(0),
	
	Datetime21 = tools:get_datetime(-20, {{2012, 10, 31}, {10, 25, 16}}),
	Datetime22 = tools:get_datetime(10, {{2012, 10, 31}, {23, 59, 56}}),
	
	[?_assert(Year > 0),
	?_assert(Month > 0),
	?_assert(Day > 0),
	?_assert(Hour >= 0),
	?_assert(Minute >= 0),
	?_assert(Second >= 0),
	?_assert(Datetime11 < Datetime12),
	?_assertEqual(Datetime21, {{2012, 10, 31}, {10, 24, 56}}),
	?_assertEqual(Datetime22, {{2012, 11, 1}, {0, 0, 6}})].
	

%% ===================================================================
%% Local Functions
%% ===================================================================



