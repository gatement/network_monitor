-module(rrdtool_helper).
-export([get_rrdtool_create_ping_cmd/1,
			run_rrdtool_create_ping_cmd/1,
			get_rrdtool_update_ping_cmd/3,
			run_rrdtool_update_ping_cmd/3,
			run_rrdtool_graph_ping_loss_cmd/3,
			run_rrdtool_graph_ping_time_cmd/3]).

-include("nm_manager.hrl").

%%
%% API Functions
%%

get_rrdtool_create_ping_cmd(AgentMibId) ->
	config_helper:get_rrdtool_exe_path() ++ " create " 
		++ config_helper:get_rrdtool_db_dir() ++ "/ping_" ++erlang:integer_to_list(AgentMibId) ++ ".rrd"
		++ " --start N  --step 300" 
		++ " DS:loss:GAUGE:600:U:U"
		++ " DS:time:GAUGE:600:U:U"
		++ " RRA:AVERAGE:0.5:1:17856".

run_rrdtool_create_ping_cmd(AgentMibId) ->
	os:cmd(get_rrdtool_create_ping_cmd(AgentMibId)).
		

get_rrdtool_update_ping_cmd(AgentMibId, Loss, Time) ->
	config_helper:get_rrdtool_exe_path() ++ " update " 
		++ config_helper:get_rrdtool_db_dir() ++ "/ping_" ++erlang:integer_to_list(AgentMibId) ++ ".rrd"
		++ " --template loss:time " 
		++ tools:now_epoch_seconds_str() ++ ":" ++ Loss ++ ":"  ++ Time.

run_rrdtool_update_ping_cmd(AgentMibId, Loss, Time) ->
	os:cmd(get_rrdtool_update_ping_cmd(AgentMibId, Loss, Time)).


run_rrdtool_graph_ping_loss_cmd(AgentMibs, From, To) ->
	Params = get_rrdtool_graph_ping_time_str(From, To) ++ get_rrdtool_graph_ping_def_str_loss(AgentMibs, []) ++ get_rrdtool_graph_ping_line_str_loss(AgentMibs, []),
	FileName = io_lib:format("/ping_loss_~s.png", [tools:md5_str(Params)]),
	run_rrdtool_graph_ping_cmd(Params, FileName).


run_rrdtool_graph_ping_time_cmd(AgentMibs, From, To) ->
	Params = get_rrdtool_graph_ping_time_str(From, To) ++ get_rrdtool_graph_ping_def_str_time(AgentMibs, []) ++ get_rrdtool_graph_ping_line_str_time(AgentMibs, []),
	FileName = io_lib:format("/ping_loss_~s.png", [tools:md5_str(Params)]),
	run_rrdtool_graph_ping_cmd(Params, FileName).


%%
%% Local Functions
%%

run_rrdtool_graph_ping_cmd(Params, FileName) ->
	FullFileName = config_helper:get_rrdtool_img_abs_dir() ++ FileName,

	case filelib:is_file(FullFileName) of
		true -> do_nothing;
		false -> 
			Cmd =  lists:flatten(io_lib:format("~s graph ~s ~s", [config_helper:get_rrdtool_exe_path(), FullFileName, Params])),
			os:cmd(Cmd)
	end,

	 lists:flatten(config_helper:get_rrdtool_img_rel_dir()  ++ FileName).


get_rrdtool_graph_ping_time_str(From, To) ->
	io_lib:format(" --start \"~s\" --end \"~s\"", [From, To]).


get_rrdtool_graph_ping_def_str_loss([], Acc) ->
	Acc;
get_rrdtool_graph_ping_def_str_loss([H|T], Acc) ->
	MibId= H#nm_agent_mib.id,
	DefStr = io_lib:format(" DEF:myloss_~p=~s/ping_~p.rrd:loss:AVERAGE", [MibId, config_helper:get_rrdtool_db_dir(), MibId]),
	get_rrdtool_graph_ping_def_str_loss(T, DefStr ++ Acc).


get_rrdtool_graph_ping_line_str_loss([], Acc) ->
	Acc;
get_rrdtool_graph_ping_line_str_loss([H|T], Acc) ->
	MibId= H#nm_agent_mib.id,
	Color= H#nm_agent_mib.color,
	Name= H#nm_agent_mib.name,
	LineStr = io_lib:format(" LINE2:myloss_~p~s:\"~s\"", [MibId, Color, Name]),
	get_rrdtool_graph_ping_line_str_loss(T, LineStr ++ Acc).


get_rrdtool_graph_ping_def_str_time([], Acc) ->
	Acc;
get_rrdtool_graph_ping_def_str_time([H|T], Acc) ->
	MibId= H#nm_agent_mib.id,
	DefStr = io_lib:format(" DEF:mytime_~p=~s/ping_~p.rrd:time:AVERAGE", [MibId, config_helper:get_rrdtool_db_dir(), MibId]),
	get_rrdtool_graph_ping_def_str_time(T, DefStr ++ Acc).


get_rrdtool_graph_ping_line_str_time([], Acc) ->
	Acc;
get_rrdtool_graph_ping_line_str_time([H|T], Acc) ->
	MibId= H#nm_agent_mib.id,
	Color= H#nm_agent_mib.color,
	Name= H#nm_agent_mib.name,
	LineStr = io_lib:format(" LINE2:mytime_~p~s:\"~s\"", [MibId, Color, Name]),
	get_rrdtool_graph_ping_line_str_time(T, LineStr ++ Acc).
	