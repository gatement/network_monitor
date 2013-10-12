-module(rrdtool_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

create_test_() ->
	% Ensure not such a file
	File = get_test_rrdtool_db(),
	filelib:ensure_dir(File),
	file:delete(File),
	
	Start = tools:epoch_seconds(), 
	Step = 300,
	DSs = ["DS:loss:GAUGE:600:U:U", "DS:time:GAUGE:600:U:U"],
	RRAs = ["RRA:AVERAGE:0.5:1:17856", "RRA:AVERAGE:0.5:10:17856"],
	rrdtool:create(File, Start, Step, DSs, RRAs),
	
	[?_assert(filelib:is_regular(File))].


update_test() ->
	File = get_test_rrdtool_db(),
	Template = "loss:time",
	Value = io_lib:format("~p:10:233", [tools:epoch_seconds() + 1]),
	rrdtool:update(File, Template, Value).


graph_test_() ->	
	File = get_test_rrdtool_db(),

	Png = "./test_result/ping_loss.png",
	file:delete(Png),
	Start = tools:datetime_string('yyyyMMdd hh:mm'),
	End = tools:datetime_string('yyyyMMdd hh:mm', tools:get_datetime(3600)),
	Definitions = [io_lib:format("DEF:myloss=~s:loss:AVERAGE", [File]), io_lib:format("DEF:mytime=~s:time:AVERAGE", [File])],
	GraphElements = ["LINE2:myloss#FF0000:\"loss\"", "LINE2:mytime#00FF00:\"time\""],
	rrdtool:graph(Png, Start, End, Definitions, GraphElements),

	[?_assert(filelib:is_regular(Png))].


%% ===================================================================
%% Local Functions
%% ===================================================================

get_test_rrdtool_db() ->
	"./test_result/test.rrd".

