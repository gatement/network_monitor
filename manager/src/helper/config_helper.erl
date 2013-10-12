-module(config_helper).
-export([get_config/0,
			get_rrdtool_db_dir/0,
			get_rrdtool_exe_path/0,
			get_rrdtool_img_abs_dir/0,
			get_rrdtool_img_rel_dir/0]).

-include("nm_manager.hrl").

%%
%% API Functions
%%

get_config() ->
    {ok, [Config]} = file:consult("../nm_manager.config"),
	Config.
	
get_rrdtool_db_dir() ->
	Config = get_config(),
	Config#config.rrdtool_db_dir.

get_rrdtool_exe_path() ->
	Config = get_config(),
	Config#config.rrdtool_exe_path.

get_rrdtool_img_abs_dir() ->
	Config = get_config(),
	Config#config.rrdtool_img_abs_dir.

get_rrdtool_img_rel_dir() ->
	Config = get_config(),
	Config#config.rrdtool_img_rel_dir.

%%
%% Local Functions
%%

