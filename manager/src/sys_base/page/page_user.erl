-module(page_user).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([out/1]).


%%
%% API Functions
%%

out(Arg) ->
	out(Arg, string:tokens(Arg#arg.pathinfo, "/")).


%%
%% Local Functions
%%

out(Arg, ["logout", "noredirect"]) ->
	logout(Arg),
	{html, "ok"};


out(Arg, ["logout"]) ->
	logout(Arg),
	{redirect_local, "/login"};


out(_Arg, _) ->
	{status, 404}.


logout(Arg) ->
	model_session:delete((Arg#arg.state)#arg_state.session_id).

