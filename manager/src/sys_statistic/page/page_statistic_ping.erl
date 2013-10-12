-module(page_statistic_ping).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([out/1]).


%%
%% API Functions
%%

out(Arg) ->			
	case Arg#arg.pathinfo of
		undefined -> {html, html(Arg)};
		_ ->
			case string:tokens(Arg#arg.pathinfo, "/") of
				["time"] -> get_time(Arg);
				["loss"] -> get_loss(Arg);
				_ -> page_notfound:out(Arg)
			end
	end.


%%
%% Local Functions
%%

get_time(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	From = tools:val_by_key("fromDateTime", PostVals),
	To = tools:val_by_key("toDateTime", PostVals),
	AgentId = erlang:list_to_integer(tools:val_by_key("agentId", PostVals)),

	Mibs = model_agent_mib:get_by_agent_id(AgentId),
	Url = rrdtool_helper:run_rrdtool_graph_ping_time_cmd(Mibs, From, To),

	Result = [{"success", true}, {"data", {struct, [{"url",Url}]}}],
	{content, "application/json", json2:encode({struct, Result})}.


get_loss(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	From = tools:val_by_key("fromDateTime", PostVals),
	To = tools:val_by_key("toDateTime", PostVals),
	AgentId = erlang:list_to_integer(tools:val_by_key("agentId", PostVals)),

	Mibs = model_agent_mib:get_by_agent_id(AgentId),
	Url = rrdtool_helper:run_rrdtool_graph_ping_loss_cmd(Mibs, From, To),

	Result = [{"success", true}, {"data", {struct, [{"url",Url}]}}],
	{content, "application/json", json2:encode({struct, Result})}.


html(Arg) ->
	layout_site:header(Arg) ++
	"<div id=\"body\"><div id=\"subMenu\">" ++
	layout_statistic:menu(Arg) ++
	"</div><div id=\"content\">" ++
	content_html(Arg) ++
	"</div></div>" ++
	layout_site:footer(Arg).


content_html(Arg) ->
	QueryVals = yaws_api:parse_query(Arg),
	AgentIdStr = tools:val_by_key("agent_id", QueryVals),
	AgentId = erlang:list_to_integer(AgentIdStr),
	Agent = model_agent:get(AgentId),

	"<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/page_statistic_ping.css\" />
	<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/lib/jquery-ui/jquery-ui.custom.min.css\" />
	<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/lib/jquery-ui-timepicker-addon.css\" />
	<script src=\"/js/lib/jquery.min.js\" type=\"text/javascript\"></script>
	<script src=\"/js/lib/jquery-ui.custom.min.js\" type=\"text/javascript\"></script>
	<script src=\"/js/lib/jquery-ui-timepicker-addon.js\" type=\"text/javascript\"></script>
	<script src=\"/js/lib/spin.min.js\" type=\"text/javascript\"></script>
	<script src=\"/js/utils.js\" type=\"text/javascript\"></script>
	<script src=\"/js/page_statistic_ping.js\" type=\"text/javascript\"></script>
	<div class=\"navigationLocation\">Stastic >> Ping >> " ++ Agent#nm_agent.target_name ++ "</div>
	<div>
		<div id=\"controllersContainer\">
			<table>
				<tr>
					<td>
						<fieldset>
						<legend>Time scope</legend>
							<input type=\"hidden\" id=\"agentIdField\" value=\"" ++ AgentIdStr ++ "\" />
							<input type=\"text\" id=\"fromDateTimeField\" class=\"inputTextBox\" /> ~
							<input type=\"text\" id=\"toDateTimeField\" class=\"inputTextBox\" />
							<button id=\"showBtn\">show</button>
						</fieldset>
					</td>
					<td>
						<fieldset>
						<legend>Auto refresh</legend>				
							window: <select id=\"timeWindowSelect\">
								<option value=\"30\">30 minutes</option>
								<option value=\"60\" selected=\"selected\">1 hour</option>
								<option value=\"120\">2 hours</option>
								<option value=\"180\">3 hours</option>
								<option value=\"720\">12 hours</option>
								<option value=\"1440\">1 day</option>
								<option value=\"2880\">2 days</option>
								<option value=\"10080\">1 week</option>
								<option value=\"20160\">2 weeks</option>
								<option value=\"44640\">1 month</option>
							</select>
							<label><input type=\"checkbox\" id=\"autoRefreshCheckbox\" />auto refresh</label>
							<button id=\"refreshBtn\">refresh now</button>
						</fieldset>
					</td>
				</tr>
			</table>
		</div>
		<div id=\"imgContainer\">
			<div id=\"timeContainer\" align=\"center\"></div>
			<div class=\"chartTitle\" align=\"center\">Time (unit: ms)</div>
			<div id=\"lossContainer\" align=\"center\"></div>
			<div class=\"chartTitle\" align=\"center\">Loss (unit: %)</div>
			<div align=\"center\" class=\"lastUpdateContainer\"><label id=\"lastUpdated\"></label></div>
		</div>
	</div>".
