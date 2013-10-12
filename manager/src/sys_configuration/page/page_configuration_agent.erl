-module(page_configuration_agent).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([out/1]).


%%
%% API Functions
%%

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,
	case model_user:is_admin(UserId) of
			false -> page_notfound:out(Arg);

			true -> 
				case Arg#arg.pathinfo of
					undefined -> 
						Method = (Arg#arg.req)#http_request.method,
						if
							Method =:= 'POST' -> save(Arg);
							true -> {html, html(Arg)}
						end;
					_ ->
						case string:tokens(Arg#arg.pathinfo, "/") of
							["test", "name"] -> test_agent_name(Arg);
							["test", "desc"] -> test_agent_desc(Arg);
							_ -> page_notfound:out(Arg)
						end
				end
	end.	


%%
%% Local Functions
%%

test_agent_name(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	AgentId = erlang:list_to_integer(tools:val_by_key("id", PostVals)),
	Agent = model_agent:get(AgentId),

	Result = case snmp_helper:get_agent_name(Agent#nm_agent.user_id, Agent#nm_agent.target_name) of
		error -> [{"success", false}];
		Name -> [{"success", true}, {"data", {struct, [{"name", Name}]}}]
	end,

	{content, "application/json", json2:encode({struct, Result})}.


test_agent_desc(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	AgentId = erlang:list_to_integer(tools:val_by_key("id", PostVals)),
	Agent = model_agent:get(AgentId),
	
	Result = case snmp_helper:get_agent_desc(Agent#nm_agent.user_id, Agent#nm_agent.target_name) of
		error -> [{"success", false}];
		Desc -> [{"success", true}, {"data", {struct, [{"desc", Desc}]}}]
	end,

	{content, "application/json", json2:encode({struct, Result})}.


save(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	%io:format("~n~n~p~n~n",[PostVals]),
	IdString = tools:val_by_key("id", PostVals),
	Deleting = tools:val_by_key("deleting", PostVals),

	if
		Deleting =/= false -> % delete
			Id = erlang:list_to_integer(IdString),
			model_agent:delete(Id);

		IdString =/= false -> % save
			Id = erlang:list_to_integer(IdString),
			Enabled = case tools:val_by_key("enabled", PostVals) of
										false -> false;
										_ -> true
									end,
			Agent = model_agent:get(Id),
			model_agent:save(Agent#nm_agent{enabled = Enabled})
	end,
	
	{html, html(Arg)}.


html(Arg) ->
	layout_site:header(Arg) ++
	"<div id=\"body\"><div id=\"subMenu\">" ++
	layout_configuration:menu(Arg) ++
	"</div><div id=\"content\">" ++
	content_html(Arg) ++
	"</div></div>" ++
	layout_site:footer(Arg).


content_html(_Arg) ->
	Agents = model_agent:get_all(),

	"<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/page_configuration_agent.css\" />
	<script src=\"/js/lib/jquery.min.js\" type=\"text/javascript\"></script>
	<script src=\"/js/lib/spin.min.js\" type=\"text/javascript\"></script>
	<script src=\"/js/page_configuration_agent.js\" type=\"text/javascript\"></script>
	<div class=\"navigationLocation\">Configuration >> Agents</div>
	<table>"
	++ get_agent_html(Agents, "") ++
	"</table>".


get_agent_html([], Acc) ->
	Acc;

get_agent_html([H|T], Acc) ->
	EnabledHtml = case H#nm_agent.enabled of
										true -> " checked=\"checked\"";
										false -> ""
									end,
	Html = "
		<form method =\"post\">
		<table>
		<tr>
			<td align=\"right\"><label class=\"configurationAgentFieldText\">user id: </label><input type=\"hidden\" name=\"id\" value=\"" ++ erlang:integer_to_list(H#nm_agent.id) ++ "\" /></td>
			<td>
				<input class=\"configurationAgentField \" name=\"user_id\" readonly=\"readonly\" value=\"" ++ re:replace(erlang:atom_to_list(H#nm_agent.user_id), "\"", "\\&quot;", [global, {return,list}]) ++ "\" />
			</td>
		</tr>
		<tr>
			<td align=\"right\"><label class=\"configurationAgentFieldText\">target name: </label></td>
			<td><input class=\"configurationAgentField\" name=\"target_name\" readonly=\"readonly\" value=\"" ++ re:replace(H#nm_agent.target_name, "\"", "\\&quot;", [global, {return,list}]) ++ "\" /></td>
		</tr>
		<tr>
			<td align=\"right\"><label class=\"configurationAgentFieldText\">enabled: </label></td>
			<td><input type=\"checkbox\" class=\"configurationAgentEnabledField\" name=\"enabled\" value=\"true\"" ++ EnabledHtml ++ " /></td>
		</tr>
		<tr>
			<td align=\"right\"><label class=\"configurationAgentFieldText\">agent name: </label></td>
			<td><label class=\"agentName\"></label></td>
		</tr>
		<tr>
			<td align=\"right\"><label class=\"configurationAgentFieldText\">agent desc: </label></td>
			<td><label class=\"agentDesc\"></label></td>
		</tr>
		<tr>
			<td></td>
			<td>
				<input type=\"submit\" class=\"agentTest\" value=\"Test\" />
				<input type=\"submit\" value=\"Save\" />
				<label><input type=\"checkbox\" name=\"deleting\" value=\"true\" />delete</label>
			</td>
		</tr>
		</form>
		<tr>
			<td colspan=\"2\"><hr/></td>
		</tr>
		</table>",

	get_agent_html(T, Html ++ Acc).

	
	
