-module(page_login).

-include("yaws_api.hrl").
-include("nm_manager.hrl").


-export([out/1]).


%%
%% API Functions
%%

out(Arg) ->
	Method = (Arg#arg.req)#http_request.method,
	if
		Method =:= 'POST' -> do_login(Arg);
		true -> {html, html(Arg, "", "", "")}
	end.


%%
%% Local Functions
%%

do_login(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	Id = tools:val_by_key("id", PostVals),
	Pwd = tools:val_by_key("pwd", PostVals),
	
	QueryVals = yaws_api:parse_query(Arg),
	TargetUrl0 = tools:val_by_key("target_url", QueryVals),

	case TargetUrl0 of
		false ->
			TargetUrl = "/";
		[] ->
			TargetUrl = "/";
		_ ->
			TargetUrl = TargetUrl0
	end,
	
	case model_user:auth(Id, Pwd) of
		false ->
			{html, html(Arg, Id, Pwd, "failed to login.")};

		disabled ->
			{html, html(Arg, Id, Pwd, "user is disabled.")};

		_User ->
			% register the session
			SessionId = model_session:create(#nm_session{user_id = Id}),
			
			% cookie and redirect
			[{redirect_local, TargetUrl},
 			 yaws_api:setcookie(?SESSION_COOKIE_ID, SessionId)]
	end.


html(Arg, Id, Pwd, Msg) ->
	Vals = yaws_api:parse_query(Arg),
	TargetUrl0 = tools:val_by_key("target_url", Vals),

	case TargetUrl0 of
		false -> 
			TargetUrl = "";
		_ ->
			TargetUrl = TargetUrl0
	end,

	"<!DOCTYPE html>
	<html>
		<head>
			<title>Login - NetworkMonitor</title>
			<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
			<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/login.css\" />
			<script type=\"text/javascript\" src=\"/js/lib/jquery.min.js\"></script>
			<script type=\"text/javascript\" src=\"/js/lib/jquery.validate.min.js\"></script>
			<script type=\"text/javascript\" src=\"/js/login.js\"></script>
		</head>
		<body>
			<h2>Login</h2>
			<div id=\"content\">
				<form id=\"loginForm\" action=\"/login?target_url=" ++ TargetUrl ++ "\" method=\"post\">
					<div>
						<label for=\"id\" class=\"fieldTitle\"> User </label>
					 	<input class=\"textbox required\" type=\"text\" id=\"id\" name=\"id\" value=\"" ++ Id ++ "\" />
					</div>
					<div>
						<label for=\"pwd\" class=\"fieldTitle\"> Password </label>
						<input class=\"textbox required\" type=\"password\" id=\"pwd\" name=\"pwd\" value=\"" ++ Pwd ++ "\" />
					</div>
					<div class=\"padding\">
						<input id=\"submit\" type=\"submit\" value=\"Login\" />
					</div>
					<div class=\"padding\">
						<span class=\"msg\">" ++ Msg ++ "</span>
					</div>
				</form>
			</div>
		</body>
	</html>".
