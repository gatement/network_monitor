-module(json2_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

encode_test_() ->
	Json1 = {struct, [{"name", "Johnson"}, {"age", 22}]},
	EncodedJsonActual1 = lists:flatten(json2:encode(Json1)),
	EncodeJsonExpected1 = "{\"name\":\"Johnson\",\"age\":22}",
	
	Json2 = {array, [
		{struct, [{"name", "Johnson"}, {"age", 22}]}, 
		{struct, [{"name", "Anne"}, {"age", 21}]}
	]},
	EncodedJsonActual2 = lists:flatten(json2:encode(Json2)),
	EncodeJsonExpected2 = "[{\"name\":\"Johnson\",\"age\":22},{\"name\":\"Anne\",\"age\":21}]",
	
	[?_assertEqual(EncodedJsonActual1, EncodeJsonExpected1),
	?_assertEqual(EncodedJsonActual2, EncodeJsonExpected2)].
	
decode_string_test() ->	
	JsonString1 = "{\"name\":\"Johnson\",\"age\":22}",
	JsonString2 = "[{\"name\":\"Johnson\",\"age\":22},{\"name\":\"Anne\",\"age\":21}]",	
	
	{ok, {struct, [{"name", Name1}, {"age", Age1}]}} = json2:decode_string(JsonString1),
	{ok, {array, [{struct, [{"name", Name21}, {"age", Age21}]}, {struct, [{"name", Name22}, {"age", Age22}]}]}} = json2:decode_string(JsonString2),
	
	?assert(Name1 =:= "Johnson"),
	?assert(Age1 =:= 22),
	?assert(Name21 =:= "Johnson"),
	?assert(Age21 =:= 22),
	?assert(Name22 =:= "Anne"),
	?assert(Age22 =:= 21).
	

%% ===================================================================
%% Local Functions
%% ===================================================================



