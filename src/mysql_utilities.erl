-module(mysql_utilities).
-compile(export_all).

-include("my.hrl").
-include("networkmonitor.hrl").


initialize_mysql_client() ->
    {ok, [Config]} = file:consult("../networkmonitor.cfg"),

	my:start_client(),
	my:new_datasource(?MYSQL_DATASOURCE_ID, #datasource{
	  host = Config#config.mysql_host,
	  port = Config#config.mysql_port,
	  database = Config#config.mysql_database,
	  user = Config#config.mysql_user,
	  password = Config#config.mysql_user_password}
	).


shutdown_mysql_client() -> 
	my:close_datasource(?MYSQL_DATASOURCE_ID),
	my:stop_client().


get_connection() ->
	my:get_connection(?MYSQL_DATASOURCE_ID).


close_connection(Conn) ->
	my:close_connection(Conn).

