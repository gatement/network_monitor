-module(arg_rewriter).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([arg_rewrite/1]).


%%
%% API Functions
%%

arg_rewrite(Arg) ->
	case get_session(Arg) of
		[] ->
			do_rewrite(Arg);
		[Session] ->
			SessionId = Session#nm_session.id,
			UserId = Session#nm_session.user_id,

			model_session:update_last_active(SessionId),

			Arg#arg{ state = #arg_state{session_id = SessionId, user_id = UserId} }
		end.


%%
%% Local Functions
%%
 
% these pages must be shippable without an auth cookie
no_auth_paths() ->
	[
	"/js/lib/jquery.min.js",
	"/js/lib/jquery.validate.min.js", 
	"/login",
	"/css/login.css", 
	"/js/login.js"
	 ].

do_rewrite(Arg) ->
	Req = Arg#arg.req,
	{abs_path, Path} = Req#http_request.path,
	case lists:member(string:to_lower(Path), no_auth_paths()) of
		true ->
			% leave it if it is login urls
			Arg;
		false ->
			PathPrefix = string:to_lower(string:substr(Path, 1, 6)),
			if
				PathPrefix =:= "/login" ->
					% leave it if it is login url, like the url /logIn?name=j
					Arg;
				true ->
					Arg#arg
					{
							req = Req#http_request{path = {abs_path, "/login?target_url=" ++ yaws_api:url_encode(Path)}}
					}
			end
	end.


% if no session, return []
get_session(Arg) ->
	CookieName = ?SESSION_COOKIE_ID,
	case tools:get_cookie_val(Arg, CookieName) of
		[] -> [];
		SessionId -> 
			case model_session:get(SessionId) of
				error -> [];
				Session -> Session
			end
	end.

