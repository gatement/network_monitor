-module(page_configuration_agent_mib).

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
							["test"] -> test_mib(Arg);
							_ -> page_notfound:out(Arg)
						end
				end
	end.	


%%
%% Local Functions
%%

test_mib(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	MibId = erlang:list_to_integer(tools:val_by_key("id", PostVals)),
	Mib = model_agent_mib:get(MibId),
	Agent = model_agent:get(Mib#nm_agent_mib.agent_id),

	Result = case snmp_helper:get_oid_val(Agent#nm_agent.user_id, Agent#nm_agent.target_name, Mib#nm_agent_mib.oid) of
		error -> [{"success", false}];
		Val -> [{"success", true}, {"data", {struct, [{"value", Val}]}}]
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
			model_agent_mib:delete(Id);

		IdString =/= false -> % save
			Id = erlang:list_to_integer(IdString),
			Enabled = case tools:val_by_key("enabled", PostVals) of
										false -> false;
										_ -> true
									end,
			Name = string:strip(tools:val_by_key("name", PostVals)),
			Color = string:strip(tools:val_by_key("color", PostVals)),
			Agent = model_agent_mib:get(Id),
			model_agent:save(Agent#nm_agent_mib{name = Name, color = Color, enabled = Enabled})
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

	
content_html(Arg) ->
	QueryVals = yaws_api:parse_query(Arg),
	IdString = tools:val_by_key("id", QueryVals),
	Id = erlang:list_to_integer(IdString),
	Mibs = model_agent_mib:get_by_agent_id(Id),

	"<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/page_configuration_agent_mib.css\" />
	<script src=\"/js/lib/jquery.min.js\" type=\"text/javascript\"></script>
	<script src=\"/js/lib/spin.min.js\" type=\"text/javascript\"></script>
	<script src=\"/js/page_configuration_agent_mib.js\" type=\"text/javascript\"></script>
	<div class=\"navigationLocation\">Notification >> Agent >> MIB</div>
	<table border =\"1\" cellspacing=\"0\">
		<tr>
			<td>id</td>
			<td>oid</td>
			<td>name</td>
			<td>color</td>
			<td>type</td>
			<td>enabled</td>
			<td>deleting</td>
			<td>action</td>
			<td>result</td>
		</tr>"
		 ++ get_mib_html(Mibs, "") ++
	"</table>".


get_mib_html([], Acc) ->
	Acc;

get_mib_html([H|T], Acc) ->
	EnabledHtml = case H#nm_agent_mib.enabled of
										true -> " checked=\"checked\"";
										false -> ""
									end,
	Html = "<tr>
					<form method =\"post\">
					<td>" ++ erlang:integer_to_list(H#nm_agent_mib.id) ++ "</td>
					<td>
						<input type=\"hidden\" name=\"id\" value=\"" ++ erlang:integer_to_list(H#nm_agent_mib.id) ++ "\" />
						<input class=\"configurationAgentMibOidField\" name=\"oid\" readonly=\"readonly\" value=\"" ++ mib_to_string(H#nm_agent_mib.oid, "") ++ "\" /></td>
					<td><input class=\"configurationAgentMibNameField\" name=\"name\" value=\"" ++ re:replace(H#nm_agent_mib.name, "\"", "\\&quot;", [global, {return,list}]) ++ "\" /></td>
					<td><input class=\"configurationAgentMibColorField\" name=\"color\" value=\"" ++ re:replace(H#nm_agent_mib.color, "\"", "\\&quot;", [global, {return,list}]) ++ "\" /></td>
					<td>" ++ erlang:atom_to_list(H#nm_agent_mib.type) ++ "</td>
					<td><input type=\"checkbox\" name=\"enabled\" value=\"true\"" ++ EnabledHtml ++ " /></td>
					<td><input type=\"checkbox\" name=\"deleting\" value=\"true\" /></td>
					<td>
						<input type=\"submit\" value=\"Save\" />
						<input type=\"submit\" class=\"mibTest\" value=\"Get\" /></td>
					<td class=\"\mibTestResult\"></td>
					</form>
				</tr>",
	get_mib_html(T, Html ++ Acc).
	

mib_to_string([], Acc) ->
	Acc;

mib_to_string([H|T], Acc) ->
	mib_to_string(T, Acc ++ "." ++ erlang:integer_to_list(H)).
	
	
	
