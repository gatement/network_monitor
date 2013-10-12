-module(database_model).
-compile(export_all).


%%%======= settings ======================================
get_setting_ping_client(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'ping_client'"
	),

	Result.


get_setting_ping_client_os_type(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'ping_client_os_type'"
	),

	Result.


get_setting_ping_interval(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'ping_interval_minute'"
	),

	erlang:list_to_integer(Result) * 60000. % convert minutes to milliseconds


get_setting_ping_size_byte(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'ping_size_byte'"
	),

	erlang:list_to_integer(Result).


get_setting_ping_count_each(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'ping_count_each'"
	),

	erlang:list_to_integer(Result).


get_setting_warning_threshold_loss_percentage(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'warning_threshold_loss_percentage'"
	),

	erlang:list_to_integer(Result).


get_setting_warning_threshold_average_time(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'warning_threshold_average_time'"
	),

	erlang:list_to_integer(Result).


get_setting_warning_week_days(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'warning_week_days'"
	),

	string:tokens(Result, ",").


get_setting_warning_hours(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'warning_hours'"
	),

	string:tokens(Result, ",").


get_setting_email_account(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'email_account'"
	),

	Result.


get_setting_email_password(Conn) ->
	{_, [{rs_row_data,[Result]}]} = my:execute_query(Conn, 
	  "SELECT `value` FROM `settings` "
	  "WHERE `key` = 'email_password'"
	),

	Result.


%%%======= targets ========================================
get_targets(Conn) ->
	{_, Rows} = my:execute_query(Conn, 
	  "SELECT `name`, `host` FROM `targets` "
	  "WHERE `enabled` = 1"
	),

	Rows.


%%%======= subscribers ========================================
get_subscribers(Conn) ->
	{_, Rows} = my:execute_query(Conn, 
	  "SELECT `email` FROM `subscribers` "
	  "WHERE `enabled` = 1"
	),

	Rows.


get_not_warning_only_subscribers(Conn) ->
	{_, Rows} = my:execute_query(Conn, 
	  "SELECT `email` FROM `subscribers` "
	  "WHERE `enabled` = 1"
	  " AND `warning_only` = 0"
	),

	Rows.


%%%======= log ================================================
insert_log(Iteration, From, Target, Bytes, Count, Loss, Time, Conn) ->
	Sql = io_lib:format("INSERT INTO `log` "
		"(`iteration`, `datetime`, `from`, `target`, `bytes`, `count`, `loss`, `time_ave`) "
		"VALUES (~s,'~s','~s','~s',~s,~s,~s,~s);",
		[erlang:integer_to_list(Iteration),
		tools:datetime_string(),
		re:replace(From, "'", "''", [{return, list}]),
		re:replace(Target, "'", "''", [{return, list}]),
		erlang:integer_to_list(Bytes),
		erlang:integer_to_list(Count),
		erlang:integer_to_list(Loss),
		erlang:integer_to_list(Time)]
	),

	my:execute_query(Conn, Sql).


get_log_count(Iteration, LossThreshold, TimeThreshold, Conn) ->
	Sql = io_lib:format("SELECT COUNT(*) FROM `log` "
		"WHERE `iteration` = ~s AND (`loss` >= ~s OR `time_ave` >= ~s)",
		[erlang:integer_to_list(Iteration),
		erlang:integer_to_list(LossThreshold),
		erlang:integer_to_list(TimeThreshold)]
	),

	{_, [{rs_row_data, [Count]}]} = my:execute_query(Conn, Sql),

	Count.



get_logs(Iteration, Conn) ->
	{_, Logs} = my:execute_query(Conn, 
	  "SELECT `iteration`, `datetime`, `from`, `target`, `bytes`, `count`, `loss`, `time_ave` FROM `log` "
	  "WHERE `iteration` = " ++ erlang:integer_to_list(Iteration)
	),

	Logs.