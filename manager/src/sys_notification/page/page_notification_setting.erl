-module(page_notification_setting).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([out/1]).


%%
%% API Functions
%%

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,
	case model_user:is_admin(UserId) of
			true -> 
				Method = (Arg#arg.req)#http_request.method,
				if
					Method =:= 'POST' -> save(Arg);
					true -> {html, html(Arg)}
				end;
			false ->
				page_notfound:out(Arg)
	end.	


%%
%% Local Functions
%%

save(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	Email = string:strip(tools:val_by_key("email", PostVals)),
	EmailPassword = string:strip(tools:val_by_key("email_password", PostVals)),
	WarningThresholdPingLossPercentage = string:strip(tools:val_by_key("warning_threshold_ping_loss_percentage", PostVals)),
	WarningThresholdPingAverageTime = string:strip(tools:val_by_key("warning_threshold_ping_average_time", PostVals)),
	WarningWeekDays = string:strip(tools:val_by_key("warning_week_days", PostVals)),
	WarningHours = string:strip(tools:val_by_key("warning_hours", PostVals)),
	
	model_setting:set(#nm_setting{key = ?SETTING_NOTIFICATION_EMAIL, value = Email}),
	model_setting:set(#nm_setting{key = ?SETTING_NOTIFICATION_EMAIL_PASSWORD, value = EmailPassword}),
	model_setting:set(#nm_setting{key = ?SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_LOSS_PERCENTAGE, value = WarningThresholdPingLossPercentage}),
	model_setting:set(#nm_setting{key = ?SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_AVERAGE_TIME, value = WarningThresholdPingAverageTime}),
	model_setting:set(#nm_setting{key = ?SETTING_NOTIFICATION_WARNING_WEEK_DAYS, value = WarningWeekDays}),
	model_setting:set(#nm_setting{key = ?SETTING_NOTIFICATION_WARNING_HOURS, value = WarningHours}),

	{html, html(Arg)}.


html(Arg) ->
	layout_site:header(Arg) ++
	"<div id=\"body\"><div id=\"subMenu\">" ++
	layout_notification:menu(Arg) ++
	"</div><div id=\"content\">" ++
	content_html(Arg) ++
	"</div></div>" ++
	layout_site:footer(Arg).


content_html(_Arg) ->
	Email = model_setting:get(?SETTING_NOTIFICATION_EMAIL),
	EmailPassword = model_setting:get(?SETTING_NOTIFICATION_EMAIL_PASSWORD),
	WarningThresholdPingLossPercentage = model_setting:get(?SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_LOSS_PERCENTAGE),
	WarningThresholdPingAverageTime = model_setting:get(?SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_AVERAGE_TIME),
	WarningWeekDays = model_setting:get(?SETTING_NOTIFICATION_WARNING_WEEK_DAYS),
	WarningHours = model_setting:get(?SETTING_NOTIFICATION_WARNING_HOURS),

	"<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/page_notification_setting.css\" />
	<div class=\"navigationLocation\">Notification >> Setting</div>
	<form method =\"post\">
	<table>
		<tr>
			<td><label class=\"notificationSettingFieldText\">email:</label></td>
			<td><input class=\"notificationSettingField\" name=\"email\" value=\"" ++ re:replace(Email, "\"", "\\&quot;", [global, {return,list}]) ++ "\" /></td>
		</tr>
		<tr>
			<td><label class=\"notificationSettingFieldText\">email password:</label></td>
			<td><input class=\"notificationSettingField\" type=\"password\" name=\"email_password\" value=\"" ++ re:replace(EmailPassword, "\"", "\\&quot;", [global, {return,list}]) ++ "\" /></td>
		</tr>
		<tr>
			<td><label class=\"notificationSettingFieldText\">ping loss warning threshold (percentage):</label></td>
			<td><input class=\"notificationSettingField\" name=\"warning_threshold_ping_loss_percentage\" value=\"" ++ WarningThresholdPingLossPercentage ++ "\" /></td>
		</tr>
		<tr>
			<td><label class=\"notificationSettingFieldText\">ping average time threshold (ms):</label></td>
			<td><input class=\"notificationSettingField\" name=\"warning_threshold_ping_average_time\" value=\"" ++ WarningThresholdPingAverageTime ++ "\" /></td>
		</tr>
		<tr>
			<td><label class=\"notificationSettingFieldText\">warning week days (e.g. 1,2,3,4,5):</label></td>
			<td><input class=\"notificationSettingField\" name=\"warning_week_days\" value=\"" ++ WarningWeekDays ++ "\" /></td>
		</tr>
		<tr>
			<td><label class=\"notificationSettingFieldText\">warning hours (e.g. 9,10,11,14,15):</label></td>
			<td><input class=\"notificationSettingField\" name=\"warning_hours\" value=\"" ++ WarningHours ++ "\" /></td>
		</tr>
		<tr>
			<td></td>
			<td><input type=\"submit\" value=\"Save\" /></td>
		</tr>
	</table>
	</form>".
