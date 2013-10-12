-module(network_monitor).

-export([start/0, 
		 do/0,
		 ping/0]).


%%
%% API Functions
%%

start() ->
	process_flag(trap_exit, true),
	mysql_utilities:initialize_mysql_client(),
	do().


do() ->
	Pid = erlang:spawn_link(?MODULE, ping, []),

	receive
 		{'EXIT', Pid, normal} -> % success
			Conn = mysql_utilities:get_connection(),
			PingInterval = database_model:get_setting_ping_interval(Conn),
			mysql_utilities:close_connection(Conn),

 			tools:sleep(PingInterval),
 			do();

 		{'EXIT', Pid, Reason} -> % error
			io:format("~n======== exception ========~n"),
			io:format("~p~n~n", [Reason]),

 			tools:sleep(20000), % sleep 20 seconds
 			do()
 	end.


%%
%% Local Functions
%%

ping() ->
	Conn = mysql_utilities:get_connection(),

	%% ping
	Iteration = tools:now_integer(),
	From = database_model:get_setting_ping_client(Conn),
	OS = database_model:get_setting_ping_client_os_type(Conn),
	Count = database_model:get_setting_ping_count_each(Conn),
	Bytes = database_model:get_setting_ping_size_byte(Conn),
	Targets = database_model:get_targets(Conn),

	Fun = fun({rs_row_data, [Name, Host]}) -> 
		{Loss, Time} = ping:run(Host, Bytes, Count, OS),
		Target = Name ++ "(" ++ Host ++ ")",
		database_model:insert_log(Iteration, From, Target, Bytes, Count, Loss, Time, Conn)
	end,

	lists:foreach(Fun, Targets),


	%% send email
	LossThreshold = database_model:get_setting_warning_threshold_loss_percentage(Conn),
	TimeThreshold = database_model:get_setting_warning_threshold_average_time(Conn),
	WarningLevel = get_warning_level(Iteration, LossThreshold, TimeThreshold, Conn),
	Subscribers = case WarningLevel of
		info -> database_model:get_not_warning_only_subscribers(Conn);
		warning -> database_model:get_subscribers(Conn)
	end,

	case erlang:length(Subscribers) of
		0 -> don_not_send_email_since_no_subscriber;
		_ ->
			Emails = [Email || {rs_row_data,[Email]} <- Subscribers],
			Content = builder_email_content(Iteration, LossThreshold, TimeThreshold, Conn),
			FromEmail = database_model:get_setting_email_account(Conn),
			FromEmailPwd = database_model:get_setting_email_password(Conn),
			Subject = "Network Ping Statistics (" ++ tools:date_string() ++ ") - " ++ database_model:get_setting_ping_client(Conn),
			smtp_client:send_email(FromEmail, FromEmailPwd, Emails, Subject, Content)
	end,

	mysql_utilities:close_connection(Conn).


%% return: info/warning
get_warning_level(Iteration, LossThreshold, TimeThreshold, Conn) ->
	Count = database_model:get_log_count(Iteration, LossThreshold, TimeThreshold, Conn),
	if
		Count > 0 ->
			case is_in_warning_time(Conn) of
				true ->	warning;
				false -> info
			end;

		true -> info
	end.


is_in_warning_time(Conn) ->
	WarningWeekDays = database_model:get_setting_warning_week_days(Conn),
	WarningHours = database_model:get_setting_warning_hours(Conn),
	
	CurrentWeekDay = tools:day_of_the_week(),
	{CurrentHour, _, _} = erlang:time(),
	
	case lists:member(erlang:integer_to_list(CurrentWeekDay), WarningWeekDays) of
		true -> lists:member(erlang:integer_to_list(CurrentHour), WarningHours);
		false -> false
	end.


builder_email_content(Iteration, LossThreshold, TimeThreshold, Conn) ->
	Logs = database_model:get_logs(Iteration, Conn),

	Func = fun({rs_row_data,[_Iteration, {mysql_time,false,Year,Month,Day,Hour,Minute,Second,_},From,Target,Bytes,Count,Loss,Time]}, Content) ->
		DatetimeStr = tools:datetime_string(Year, Month, Day, Hour, Minute, Second),
		BytesStr = erlang:integer_to_list(Bytes),
		CountStr = erlang:integer_to_list(Count),
		LossStr = erlang:integer_to_list(Loss),
		TimeStr = erlang:integer_to_list(Time),
		Row = "<tr><td>" ++ DatetimeStr ++ "</td>"
			  "<td>" ++ BytesStr ++ "</td>"
			  "<td>" ++ CountStr ++ "</td>"
			  "<td>" ++ From ++ "</td>"
			  "<td>" ++ Target ++ "</td>",

		LossContent = if
			Loss >= LossThreshold -> "<td style=\"color: #FF0000;\"><strong>" ++ LossStr ++ "%</strong></td>";
			true -> "<td>" ++ LossStr ++ "%</td>"
		end,

		TimeContent = if
			Time >= TimeThreshold -> "<td style=\"color: #FF00FF;\"><strong>" ++ TimeStr ++ "ms</strong></td>";
			true -> "<td>" ++ TimeStr ++ "ms</td>"
		end,
		
		Content ++ Row ++ LossContent ++ TimeContent ++ "</tr>"
	end,

	"<html><body><table border=\"1\"  cellspacing=\"0\">"
	"<tr><td><strong>Time</strong></td><td><strong>Bytes</strong></td><td><strong>Count</strong></td><td><strong>From</strong></td><td><strong>To</strong></td><td><strong>Loss</strong></td><td><strong>Time</strong></td></tr>"
	++ lists:foldl(Func, "", Logs) ++
	"</table></body></html>".
