-module(layout_site).

-include("yaws_api.hrl").
-include("nm_manager.hrl").


-export([header/1, footer/1]).


%%
%% API Functions
%%

header(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,
	
	AdminLinks = case model_user:is_admin(UserId) of
								true -> " | <a href=\"/notification/setting\">Notification</a> | 
												<a href=\"/configuration/agent\">Configuration</a>";
								false -> ""
						end,

	"<!DOCTYPE html>
	<html>
	<head>
		<title>Network Monitor</title>
		<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />
		<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/layout_site.css\" />
	</head>
	<body>
		<div id=\"header\">
			<div>
				<h2 id=\"title\">Network Monitor</h2>
				<div id=\"userInfo\">Welcome, " ++ UserId ++ ",
						<a href=\"/user/logout\">logout</a></div>
			</div>
			<div id=\"topMenu\">
				<a href=\"/statistic/ping?agent_id=" ++ erlang:integer_to_list(model_agent:get_any_id())++ "\">Statistic</a>"
				 ++ AdminLinks ++
			"</div>
		</div>
		<hr/>".


footer(_Arg) ->
	"<div id=\"footer\"></div>
	</body>
</html>".

