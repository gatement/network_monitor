-module(layout_configuration).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([menu/1]).


%%
%% API Functions
%%

menu(_Arg) ->
	Agents = model_agent:get_all(),

	"<div><a href=\"/configuration/agent\">Agents</a></div>
	<div class=\"subSubMenu\">"
	++ get_agent_html(Agents, "") ++
	"</div>".


get_agent_html([], Acc) ->
	Acc;

get_agent_html([H|T], Acc) ->
	Html = "<div><a href=\"/configuration/agent_mib?id=" ++ erlang:integer_to_list(H#nm_agent.id) ++ "\">" ++ re:replace(H#nm_agent.target_name, "\"", "\\&quot;", [global, {return,list}]) ++ "</a></div>",
	get_agent_html(T, Html ++ Acc).

