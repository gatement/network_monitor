-module(rest_user).
-include("yaws_api.hrl").
-include("nm_manager.hrl").
-export([out/1]).


%% ===================================================================
%% API functions
%% ===================================================================

out(Arg) ->
	case Arg#arg.pathinfo of
		undefined -> {status, 404};
		PathInfo ->	out(Arg, string:tokens(PathInfo, "/"))
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================

out(Arg, ["login"]) ->	
	PostVals = yaws_api:parse_post(Arg),
	Id = proplists:get_value("id", PostVals),
	Pwd = proplists:get_value("pwd", PostVals),

	QueryVals = yaws_api:parse_query(Arg),
	TargetUrl = case proplists:get_value("target_url", QueryVals) of
		undefined -> "/";
		Val -> Val
	end,

	case model_user:auth(Id, Pwd) of
		false -> 
			Msg = yaws_api:url_encode("Wrong username/password."),
			Url = lists:flatten(io_lib:format("/login.yaws?id=~s&target_url=~s&msg=~s", [Id, TargetUrl, Msg])),
			{redirect_local, Url};
		disabled -> 
			Msg = yaws_api:url_encode("User is disabled, please connect administrator."),
			Url = lists:flatten(io_lib:format("/login.yaws?id=~s&target_url=~s&msg=~s", [Id, TargetUrl, Msg])),
			{redirect_local, Url};
		_User ->
			Url = TargetUrl,
			{redirect_local, Url}
	end;


out(Arg, ["logout", "noredirect"]) ->
	logout(Arg),
	{html, "ok"};


out(Arg, ["logout"]) ->
	logout(Arg),
	{redirect_local, "/login.yaws"};


out(_Arg, _) ->
	{status, 404}.


logout(Arg) ->
	model_session:delete((Arg#arg.state)#arg_state.session_id).
