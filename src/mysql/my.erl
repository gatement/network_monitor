%%
%% Copyright (C) 2010-2012 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @since 2010-11-05
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc 
%% This is a main module of Erlang MySQL client. 
%% The module contains client API functions for all allowed operations.  
%%
%% @reference [http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol]
%% @headerfile "my.hrl"

-module(my).

%%
%% Include files
%%
-include("my.hrl").

%%
%% Import modules
%%
-import(send_cmd, []).
-import(statement, []).
-import(conn, []).

%%
%% Exported Functions
%%
-export([
  start_client/0,
  stop_client/0,
  
  new_datasource/1,
  new_datasource/2,
  change_user/4, 
  select_db/2, 
  close_datasource/1,

  get_connection/1,
  connection_record/1, 
  close_connection/1, 
  remove_connection/1, 
  ping/1, 
  execute_query/2, 
  get_field_list/3, 
  prepare_statement/2, 
  get_prepared_statement_handle/2, 
  send_statement_long_parameter/4, 
  execute_statement/4, 
  execute_statement/6, 
  close_statement/2, 
  reset_statement/2, 
  fetch_statement/4, 
  transaction/2
]).

%%
%% API Functions
%%

%% @spec start_client() -> ok | {error, Reason}
%% 
%% @doc The function loads and starts OTP application named 'mysql_client'
%%
start_client() ->
  application:load(
    {  application, 
      mysql_client,
      [
        {description, "MySQL native client"},
        {vsn, "1.1.3"},
        {  modules, 
          [  binary_row_data,
            conn_pool_srv,
            conn_srv,
            conn_sup,
            conn,
            datasource_sup,
            my_app,
            my_io,
            my,
            packet_parser,
            response,
            send_cmd,
            statement,
            string_row_data,
            util
          ]
        },
        {registered, [datasource]},
        {applications, [kernel, stdlib]},
        {mod, {my_app, []}}
      ]
    }
  ),
  application:start(mysql_client)
.

%% @spec stop_client() -> ok | {error, Reason}
%%
%% @doc The function stops and unload 'mysql_client' application.
%%
stop_client() ->
  application:stop(mysql_client),
  application:unload(mysql_client)
.

%% @spec new_datasource(DS_name::atom(), DS_def::#datasource{}) -> Result
%% Result = {ok, Pid} | #mysql_error{}
%% 
%% @doc the function starts new child process registered as DS_name and implements
%% datasorce (or connection pool) functionality. The datasource process is controlled by
%% supervisor 'datasource_sup'.
%%
new_datasource(DS_name, DS_def) ->
%  io:format(user, " >>> my:new_datasource/2: ~p ~p~n", [DS_name, DS_def]),
  Child_spec = {
    DS_name,
    {  conn_sup,
      start_link,
      [DS_name, DS_def#datasource{name = DS_name}]
    },
    temporary,
    infinity,
    supervisor,
    [conn_sup]
  },
  R = supervisor:start_child(datasource_sup, Child_spec),
%% >>> debug
%  io:format(user, "     returns: ~p~n", [R]),
%  io:format(user, "     from name to pid: ~p~n", [whereis(DS_name)]),
%  io:format(user, "     children list:~p~n", [supervisor:which_children(?SERVER)]),
%% <<< debug
  case R of
    {ok, _Pid} -> R;
    {error, Reason} -> #mysql_error{type = datasource, 
                                    source = "datasource_sup:start_child/2",
                                    message = Reason}
  end
.

new_datasource(DS_def) -> new_datasource(DS_def#datasource.name, DS_def)
.

%% @spec close_datasource(DS_name::atom()) -> ok | {error,Error}
%%
%% @doc Stops datasource DS_name and disposes all linked resources.
%%
close_datasource(DS_name) ->
%  io:format(user, " >>> my:close_datasource/1: ~p ~n", [DS_name]),
%  io:format(user, "     children list:~p~n", [supervisor:which_children(DS_name)]),
  conn_pool_srv:stop(DS_name),
  supervisor:terminate_child(datasource_sup, DS_name),
  supervisor:delete_child(datasource_sup, DS_name)
.

%% @spec get_connection(DS_name::atom()) -> pid() | Error
%% 
%% @doc Provides with new connection process under conn_sup supervisor. The new
%% connection process is used old idle tcp connection socket or creates new connection
%% socket to MySQL server.
%%
get_connection(DS_name) ->
%  io:format(user, " >>> my:get_connection/1~n", []),
  case conn_sup:start_child(DS_name) of
    {ok, Pid} -> Pid;
    {error, {Error, _Child_spec}} -> Error;
    {error, Error} -> Error
  end
.

%% @spec connection_record(Connection_name::pid()) -> #connection{} | #mysql_error{}
%% 
%% @doc Returns #connection{} record decribed given connection. 
%%
connection_record(Connection_name) ->
  gen_server_call(Connection_name, connection_record)
.

%% @spec change_user(Connection_name::pid(), User::string(), Password::string(), Db::string) -> #connection{} | #mysql_error{}
%%
%% @doc Changes user for the connection. Change concerns only
%% given connection not top database. After closing the connection will be removed
%% from connection pool.
%% @see conn:change_user/4
change_user(Connection_name, User, Password, Db) -> 
  gen_server_call(Connection_name, {change_user, User, Password, Db})
.

%% @spec close_connection(Connection_name::pid()) -> ok
%% 
%% @doc Closes the connection to DB. Connection socket is not destroyed but stored in
%% connection pool idle list.
close_connection(Connection_name) -> 
  conn_sup:stop_child(Connection_name)
%  ok = gen_server_call(Connection_name, close_connection)
%  gen_server:cast(Connection_name, stop)
.

%% @spec remove_connection(Connection_name::pid()) -> ok
%% 
%% @doc The command ask MySQL server to close the current connection session.
%% Corresponded process is stoped and socket is closed.
%%
remove_connection(Connection_name) ->
  conn_sup:stop_child(Connection_name, true)
%  ok = gen_server_call(Connection_name, quit)
%  gen_server:cast(Connection_name, stop)
.

%% @spec ping(Connection_name::pid()) -> {Metadata::#metadata{}, [#ok_packet{}]} | #mysql_error{}
%%
%% @doc Pings MySQL server to refresh the current connection session and disallow timeout closing. 
%%
ping(Connection_name) ->
  gen_server_call(Connection_name, ping)
.

%% @spec execute_query(Connection_name::pid(), Query::string()) -> Result
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(#rs_row_data{})
%%
%% @doc Executes SQL query and return a tuple of field metadata information and list of result rows.
%% @see response:handle_response/2
%%
execute_query(Connection_name, Query) ->
  gen_server_call(Connection_name, {execute_query, Query})
.

%% @spec select_db(Connection_name::pid(), DBName::string()) -> Result
%% Result = {Metadata::#metadata{}, [#ok_packet{}]} | #mysql_error{}
%%
%% @doc Selects or changes current database (or schema). Change concerns only
%% given connection not top database. After closing the connection will be removed
%% from connection pool.
%%
select_db(Connection_name, DBName) ->
  gen_server_call(Connection_name, {select_db, DBName})
.

%% @spec get_field_list(Connection_name::pid(), Table_name::string(), Column_name::string()) -> Result 
%% Result = {Metadata::#metadata{}, []} | #mysql_error{}
%%
%% @doc Returns list of field metadata records for table 'Table_name'. Column_name is requalar 
%% expression using symbols % and _ to filter column names for return.
%%
get_field_list(Connection_name, Table_name, Column_name) ->
  gen_server_call(Connection_name, {get_field_list, Table_name, Column_name})
.

%% @spec prepare_statement(Connection_name::#connection{}, Statement::string()) -> Result
%% Result = {Metadata::#metadata{}, [#ok_stmt_packet{}]} | #mysql_error{}
%% 
%% @doc Send command to prepare statement. Returns #ok_stmt_packet.stmt_handle handle 
%% of prepared statement that will be used for followed operation with the statement.
%%
prepare_statement(Connection_name, Statement) ->
  gen_server_call(Connection_name, {prepare_statement, Statement})
.

%% @spec get_prepared_statement_handle(Connection_name::pid(), Statement::string()) -> integer()
%% 
%% @doc The same is prepare_statement/2 but it has short return: just a handle 
%% of a prepared statement.
%%
get_prepared_statement_handle(Connection_name, Statement) ->
  {_,[#ok_stmt_packet{stmt_handle = Handle}|_]} = 
    gen_server_call(Connection_name, {prepare_statement, Statement}),
  Handle
.

%% @spec send_statement_long_parameter(Connection_name, Handle, Parameter_index, Parameter) -> any()
%% Connection_name = pid()
%% Handle = integer()
%% Parameter_index = integer()
%% Parameter = binary()
%% 
%% @doc Sends parameter of prepered statement to SQL server before statement execution.
%%
send_statement_long_parameter(Connection_name, Handle, Parameter_index, Parameter) ->
  gen_server:cast(Connection_name, {send_statement_long_parameter, Handle, Parameter_index, Parameter})
.

%% @spec execute_statement(Connection_name, Handle, Parameter_types, Parameters) -> Result
%% Connection_name = pid()
%% Handle = integer()
%% Parameter_types = list(integer())
%% Parameters = list(term())
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(#rs_row_data{})
%%
%% @doc Executes prepared statement represented with Handler. Parameters will replace '?' placeholders in prepared statement. 
%% Parameter_types list passes types for each parameter. 
%% Returns result as a list of data rows.
%% @see execute_statement/5
execute_statement(Connection_name, Handle, Parameter_types, Parameters) ->
  execute_statement(Connection_name, Handle, Parameter_types, Parameters, ?CURSOR_TYPE_NO_CURSOR, true)
.

%% @spec execute_statement(Connection_name, Handle, Parameter_types, Parameters, Flags, New) -> Result
%% Connection_name = pid()
%% Handle = integer()
%% Parameter_types = list(integer())
%% Parameters = list(term())
%% Flags = integer()
%% New = boolean()
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(#rs_row_data{})
%% 
%% @equiv execute_statement/4
%% @doc The same as execute_statement/4 except parameter New. The New equals true forces command send parameter types to server.
execute_statement(Connection_name, Handle, Parameter_types, Parameters, Flags, New) ->
  gen_server_call(Connection_name, {execute_statement, Handle, Parameter_types, Parameters, Flags, New})
.

%% @spec close_statement(Connection_name::pid(), Handle::integer()) -> {Metadata::#metadata{}, []} | #mysql_error{}
%%
%% @doc The command closes prepared statement and frees corresponded resources on server.
%%
close_statement(Connection_name, Handle) ->
  gen_server_call(Connection_name, {close_statement, Handle})
.

%% @spec reset_statement(Connection_name::pid(), Handle::integer()) -> {Metadata::#metadata{}, []} | #mysql_error{}
%%
%% @doc Resets prepared statement and removes all cashed data on server.
%%
reset_statement(Connection_name, Handle) ->
  gen_server_call(Connection_name, {reset_statement, Handle})
.

%% @spec fetch_statement(Connection_name, Handle, Metadata, Row_number) -> Result
%% Connection_name = pid()
%% Handle = integer()
%% Metadata = #metadata{}
%% Row_number = integer()
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(#rs_row_data{})
%%
%% @doc Fetchs data from a cursor that was created by statement execution.
%%
fetch_statement(Connection_name, Handle, Metadata, Row_number) ->
  gen_server_call(Connection_name, {fetch_statement, Handle, Metadata, Row_number})
.

%% @spec transaction(Connection_name::pid(), F::fun()) -> any() | #mysql_error{type=transaction}
%% 
%% @doc executes a transaction. F represents a function with sequence of queries under the transaction management.
%% (f.e F = fun(Connection) -> my:execute_query(...), ... end). The function returns result of F/1 if transaction is successful
%% and returns #mysql_error{} if transaction was rollbacked.
%%
transaction(Connection_name, F) ->
  my:execute_query(Connection_name, "START TRANSACTION"),
  R =
  try 
    F(Connection_name)
  catch
    _:Err -> #mysql_error{type=transaction, message=Err}
  end,
%  io:format(">>> transaction ~p~n", [R]),
  case R of
    #mysql_error{} -> my:execute_query(Connection_name, "ROLLBACK"), R#mysql_error{type=transaction};
    _ -> my:execute_query(Connection_name, "COMMIT"), R
  end
.

%%
%% Private functions
%%

gen_server_call(Conn_pid, Request) ->
  try gen_server:call(Conn_pid, Request, ?GEN_SERVER_TIMEOUT)
  catch
    exit:Reason -> #mysql_error{type = connection, 
              source = "Exit is catched in my:gen_server_call/2.",
              message = Reason}
  end
.