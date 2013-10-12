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

%% @since 2011-08-08
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc 
%% The module implements connection gen_server. The module provides callback functions
%% for all operations with connection to MySQL server.
%%
%% @reference [http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol]
%% 

-module(conn_srv).
-behaviour(gen_server).

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
  open_connection/1
]).

%%
%% Internal exports. (gen_server callbacks)
%%
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2,
  terminate/2, 
  code_change/3
]).

%%
%% API Functions
%%

%% @spec open_connection(Args::term()) -> #connection{} 
%% 
%% @doc Open ip/tcp socket connection to remote MySQL server. The function returns a record 
%% that contains all information about the connection.
%% 
open_connection(Args) ->
%  io:format(user, " >>> conn_srv:open_connection/1~n", []),
  case R = gen_server:start_link(?MODULE, Args, []) of
%% Set transaction isolation level:
    {ok, Conn} ->
      Connection = my:connection_record(Conn),
      Query_part =
      case (Connection#connection.client_info)#client_options.trans_isolation_level of
        read_uncommitted -> "READ UNCOMMITTED";
        read_committed -> "READ COMMITTED";
        serializable -> "SERIALIZABLE";
        repeatable_read -> "REPEATABLE READ";
        _ -> "REPEATABLE READ"
      end,
      case gen_server:call(Conn,
              {execute_query, "SET SESSION TRANSACTION ISOLATION LEVEL " ++ Query_part},
              ?GEN_SERVER_TIMEOUT)
      of
        #mysql_error{} ->
          io:format(user, "can not set transaction isolation level = ~p~n", [Query_part]); %% TODO : how transit the error to client?
        _ -> ok
      end,
      R;
    _ -> R
  end
.

%%
%% Server callback functions
%%

%% @spec init({DS_def::#datasource{}, Reference::atom()}) -> Return
%% Return = {ok, State::term()} | {stop, Error::#mysql_error{}}
%%
%%% @doc Initiates the server.
%%
init({DS_name, Reference}) ->
%  io:format(user, " >>> my:init/1~n", []),
  case R = conn_pool_srv:get(DS_name, self()) of
    {ok, Conn} -> {ok, {DS_name, Reference, Conn}};
    _ -> R
  end
.

%% @spec handle_call(Request::term(), From::pid(), State::term()) -> Return 
%% Return = {reply, Reply, State::term()}          |
%%          {reply, Reply, State::term(), Timeout} |
%%          {noreply, State::term()}               |
%%          {noreply, State::term(), Timeout}      |
%%          {stop, Reason, Reply, State::term()}   |
%%          {stop, Reason, State::term()}
%% 
%% @doc Handling call messages.
%%
handle_call(connection_record, _From, {_DS_name, _Reference, Connection} = State) ->
  {reply, Connection, State};

handle_call(connection_state, _From, State) ->
  {reply, State, State};

handle_call(ping, _From, {_DS_name, _Reference, Connection} = State) ->
  Value = send_cmd:send_cmd_packet(Connection, <<14:8>>),
  {reply, Value, State};

handle_call({change_user, User, Password, Db}, _From, {DS_name, Reference, Connection}) ->
  New_connection = conn:change_user(Connection, User, Password, Db),
  conn_pool_srv:remove(Connection),
  {reply, ok, {DS_name, Reference, New_connection}};

handle_call({execute_query, Query}, _From, {_DS_name, _Reference, Connection} = State) ->
  Value = send_cmd:send_cmd_packet(Connection, <<3:8, (list_to_binary(Query))/binary>>),
  {reply, Value, State};

handle_call({select_db, DBName}, _From, {DS_name, Reference, Connection}) ->
  Value = send_cmd:send_cmd_packet(Connection, <<2:8, (list_to_binary(DBName))/binary>>),
  conn_pool_srv:remove(Connection),
  {reply, Value, {DS_name, Reference, Connection#connection{dbname = DBName}}};

handle_call({get_field_list, Table_name, Column_name}, _From, {_DS_name, _Reference, Connection} = State) ->
  Value = send_cmd:send_cmd_packet(Connection, <<4:8, (list_to_binary(Table_name))/binary, 0:8, (list_to_binary(Column_name))/binary>>),
  {reply, Value, State};

handle_call({prepare_statement, Statement}, _From, {_DS_name, _Reference, Connection} = State) ->
  Value = send_cmd:send_cmd_packet(Connection, <<22:8, (list_to_binary(Statement))/binary>>, {0, prepare}),
  {reply, Value, State};

handle_call({execute_statement, Handle, Parameter_types, Parameters, Flags, New}, _From, {_DS_name, _Reference, Connection} = State) ->
  case New of 
    true -> Is_new = 1, Par_types_binary = statement:pack_types(Parameter_types); 
    false -> Is_new = 0, Par_types_binary = <<>> 
  end,
  NULL_bit_map = statement:pack_null_bit_map(Parameter_types, Parameters),
  Value =
    case Par_binary = statement:pack_parameters(Parameter_types, Parameters) of
      #mysql_error{} -> Par_binary;
      _ ->  send_cmd:send_cmd_packet(Connection, <<23:8, Handle:32/little-integer, 
              Flags:8/integer, 1:32/little-integer, NULL_bit_map/binary, 
              Is_new:8, Par_types_binary/binary, Par_binary/binary>>)
    end,
  {reply, Value, State};

handle_call({close_statement, Handle}, _From, {_DS_name, _Reference, Connection} = State) ->
  Value = send_cmd:send_cmd_packet_no_resp(Connection, <<25:8, Handle:32/little-integer>>),
  {reply, Value, State};

handle_call({reset_statement, Handle}, _From, {_DS_name, _Reference, Connection} = State) ->
  Value = send_cmd:send_cmd_packet(Connection, <<26:8, Handle:32/little-integer>>, 0),
  {reply, Value, State};

handle_call({fetch_statement, Handle, Metadata, Row_number}, _From, {_DS_name, _Reference, Connection} = State) ->
  Value = send_cmd:send_cmd_packet(Connection, <<28:8, Handle:32/little-integer, Row_number:32/little-integer>>, Metadata, {0, fetch}),
  {reply, Value, State};

handle_call(close_connection, _From, {_DS_name, _Reference, Connection} = State) ->
  Pool_name = Connection#connection.pool_name,
  Socket = Connection#connection.socket,
  gen_tcp:controlling_process(Socket, whereis(Pool_name)),
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  Value = null,
  {reply, Value, State}
.

%% @spec handle_cast(Request::term(), State::term()) -> Return
%% Return = {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%%
%% @doc Handling cast messages.
%%
handle_cast({send_statement_long_parameter, Handle, Parameter_index, Parameter}, {_DS_name, _Reference, Connection} = State) ->
  send_cmd:send_cmd_packet_no_resp(Connection, <<24:8, Handle:32/little-integer, 
    Parameter_index:16/little-integer, Parameter/binary>>),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}
.

%% @spec handle_info(Info::term(), State::term()) -> Return
%% Return = {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%%
%% @doc Handling all non call/cast messages.
%%
handle_info(timeout, State) -> 
  io:format(user, " ### handle_info timeout~n", []),
  {stop, normal, State};

handle_info(Info, State) -> 
  io:format(user, " ### handle_info ~p ~n", [Info]),
  {stop, normal, State}
.

%% @spec terminate(Reason::term(), State::term()) -> any() 
%% 
%% @doc Shutdown the server and returns any (ignored by gen_server).
%%
terminate(_Reason, _State) -> 
%  io:format(user, " ### terminate ~p;~p~n", [_Reason, self()]),
  ok
.

%% @spec code_change(OldVsn, State, Extra) -> {ok, State}
%% 
%% @doc Converts process state when code is changed.
%%
code_change(_OldVsn, State, _Extra) -> {ok, State}.
