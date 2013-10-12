-module(rrdtool).
-export([create/5,
	update/3,
	graph/5]).

%% ===================================================================
%% API functions
%% ===================================================================

create(File, Start, Step, DSs, RRAs) ->
	Exe = manager_config:get(rrdtool_exe),
	TimeStr = io_lib:format("--start ~p --step ~p", [Start, Step]), 
	DSsStr = tools:list_string_to_string(DSs, " ", []),
	RRAsStr = tools:list_string_to_string(RRAs, " ", []),
	Cmd = io_lib:format("~s create ~s ~s ~s ~s", [Exe, File, TimeStr, DSsStr, RRAsStr]),
	os:cmd(lists:flatten(Cmd)).


update(File, Template, Value) ->
	Exe = manager_config:get(rrdtool_exe),
	TemplateStr = io_lib:format("--template ~s", [Template]),
	Cmd = io_lib:format("~s update ~s ~s ~s", [Exe, File, TemplateStr, Value]),
	os:cmd(lists:flatten(Cmd)).


graph(Png, Start, End, Definitions, GraphElements) ->
	Exe = manager_config:get(rrdtool_exe),
	DefinitionsStr = tools:list_string_to_string(Definitions, " ", []),
	GraphElementsStr = tools:list_string_to_string(GraphElements, " ", []),
	Cmd = io_lib:format("~s graph ~s --start ~p --end ~p ~s ~s", [Exe, Png, Start, End, DefinitionsStr, GraphElementsStr]),
	os:cmd(lists:flatten(Cmd)).
	

%% ===================================================================
%% Local Functions
%% ===================================================================
