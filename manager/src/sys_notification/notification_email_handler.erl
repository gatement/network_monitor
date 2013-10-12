-module(notification_email_handler).

-include("nm_manager.hrl").

-export([handle_ping/3]).


%%
%% API Functions
%%

handle_ping(Agent, Mib, Val) ->
	[LossStr, TimeStr] = string:tokens(Val, ","),
	Loss = erlang:list_to_integer(LossStr),
	Time = erlang:list_to_integer(TimeStr),

	LossThreshold = erlang:list_to_integer(model_setting:get(?SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_LOSS_PERCENTAGE)),
	TimeThreshold = erlang:list_to_integer(model_setting:get(?SETTING_NOTIFICATION_WARNING_THRESHOLD_PING_AVERAGE_TIME)),
	WarningLevel = get_warning_level(LossThreshold, TimeThreshold, Loss, Time),
	Subscribers = case WarningLevel of
		info -> model_notification_subscriber:get_enabled_not_warning_only();
		warning -> model_notification_subscriber:get_enabled()
	end,

	case erlang:length(Subscribers) of
		0 -> don_not_send_email_since_no_subscriber;
		_ ->
			Emails = [Subscriber#nm_notification_subscriber.email || Subscriber <- Subscribers],
			Content = builder_email_content(LossThreshold, TimeThreshold, Loss, Time, Agent, Mib),
			FromEmail = model_setting:get(?SETTING_NOTIFICATION_EMAIL),
			FromEmailPwd = model_setting:get(?SETTING_NOTIFICATION_EMAIL_PASSWORD),			
			PingClient = Agent#nm_agent.target_name,
			Subject = io_lib:format("Network Ping Statistics (~s) - ~s", [tools:date_string(), PingClient]),
			smtp_client:send_email(FromEmail, FromEmailPwd, Emails, Subject, Content)
	end.

	
%%
%% Local Functions
%%

%% return: info/warning
get_warning_level(LossThreshold, TimeThreshold, Loss, Time) ->
	if
		Time >= TimeThreshold ->
			case is_in_warning_time() of
				true ->	warning;
				false -> info
			end;
		Loss >= LossThreshold ->
			case is_in_warning_time() of
				true ->	warning;
				false -> info
			end;
		true -> info
	end.

%% return: true/false
is_in_warning_time() ->
	WarningWeekDays = string:tokens(model_setting:get(?SETTING_NOTIFICATION_WARNING_WEEK_DAYS), ","),
	WarningHours = string:tokens(model_setting:get(?SETTING_NOTIFICATION_WARNING_HOURS), ","),
	
	CurrentWeekDay = tools:day_of_the_week(),
	{CurrentHour, _, _} = erlang:time(),
	
	case lists:member(erlang:integer_to_list(CurrentWeekDay), WarningWeekDays) of
		true -> lists:member(erlang:integer_to_list(CurrentHour), WarningHours);
		false -> false
	end.


builder_email_content(LossThreshold, TimeThreshold, Loss, Time, Agent, Mib) ->
	Now = tools:datetime_string(),
	From = Agent#nm_agent.target_name,
	To = Mib#nm_agent_mib.name,
	LossStr = erlang:integer_to_list(Loss),
	TimeStr = erlang:integer_to_list(Time),

	LossContent = if
		Loss >= LossThreshold -> "<td style=\"color: #FF0000;\"><strong>" ++ LossStr ++ "%</strong></td>";
		true -> "<td>" ++ LossStr ++ "%</td>"
	end,

	TimeContent = if
		Time >= TimeThreshold -> "<td style=\"color: #FF00FF;\"><strong>" ++ TimeStr ++ "ms</strong></td>";
		true -> "<td>" ++ TimeStr ++ "ms</td>"
	end,

	io_lib:format("<html><body><table border=\"1\"  cellspacing=\"0\">"
	"<tr><td><strong>Time</strong></td><td><strong>From</strong></td><td><strong>To</strong></td><td><strong>Loss</strong></td><td><strong>Time</strong></td></tr>"
	"<tr><td>~s</td><td>~s</td><td>~s</td>~s~s</tr>"
	"</table></body></html>", [Now, From, To, LossContent, TimeContent]).

