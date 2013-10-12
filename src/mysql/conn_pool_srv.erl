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

%% @since 2011-08-17
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc The module is generic server that implements an operations with pool of connection objects.
%% Connection pool keeps connections in two lists: active (or busy) connections and
%% idle connections these are ready for reassigment. The module allows add and remove connection.
%% Close operation move given connection from busy list to idle list. Additionally 
%% connection pool keeps associated datasource description. 

-module(conn_pool_srv).
-behaviour(gen_server).

%%
%% Include files
%%
-include("my.hrl").

%%
%% External exports
%%
-export([
  start_link/2,
  stop/1,
  close/1,
  get/2,
  remove/1,
  dispose/1,
  count/1
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

%% @spec start_link(DS_name::atom(), DS_def::#datasource{}) -> Result
%% Result = {ok,Pid} | ignore | {error,Error}
%%
%% @doc Creates and starts gen_server that implements connection store functionality
%% for DS_name datasource. DS_def is defenition of the datasource that will be used
%% when new connection objects are created.
%%
start_link(DS_name, DS_def) ->
  gen_server:start_link({local, name(DS_name)}, ?MODULE, DS_def, [])
.

%% @spec stop(DS_name::atom()) -> ok
%%
%% @doc Closes the all connections from the pool and stop this gen_server.
%%
stop(DS_name) ->
  Pool_name = name(DS_name),
  gen_server:call(Pool_name, destroy),
  gen_server:cast(Pool_name, stop)
.

%% @spec close(Conn::#connection{}) -> ok
%% 
%% @doc Moves Connection from busy state to idle state. The connection is still
%% ready for use and has open and active socket connection to MySQL server.
%%
close(Conn) ->
  gen_server:call(Conn#connection.pool_name, {close, Conn})
.

%% @spec get(DS_name::atom(), Conn_pid::pid()) -> Result
%% Result = {ok, #connection{}} | {stop, #connection{}}
%%
%% @doc Tryes to retreive any connection from list of idle connections. If idle list
%% is empty server creates and returns new one. 
%% DS_name is name of corresponded data source.
%% Conn_pid is a pid of connection gen_server process to those we have to delegate
%% ownership of connection tcp socket.
%%
get(DS_name, Conn_pid) ->
  Pool_name = name(DS_name),
  gen_server:call(Pool_name, {get, Pool_name, Conn_pid})
.

%% @spec remove(Conn::#connection{}) -> ok
%% @doc Removes connection from pool but not close associated tcp socket.
%%
remove(Conn) ->
  gen_server:call(Conn#connection.pool_name, {remove, Conn})
.

%% @spec dispose(Conn::#connection{}) -> ok
%% @doc Removes connection from pool and close connection tcp socket.
%%
dispose(Conn) ->
  gen_server:call(Conn#connection.pool_name, {dispose, Conn})
.

%% @spec count(DS_name::atom()) -> {integer(), integer()}
%%
%% @doc Returns status of pool. Status represents tuple of two integer: 
%% lengths of busy and idle lists of connection.
%%
count(DS_name) ->
  gen_server:call(name(DS_name), count)
.

%%
%% Server callback functions
%%

%% @spec init(DS_def::#datasource{}) -> {ok, State}
%% State = {Busy, Idle, DS_def}
%% Busy = list(#connection{})
%% Idle = list(#connection{})
%%
%% @doc Initiates the server.
%%
init(DS_def) ->
  {ok, {[], [], DS_def}}
.

%% @spec handle_call(Request::term(), From::pid(), State::term()) -> Return 
%% Return = {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   |
%%          {stop, Reason, State}
%% 
%% @doc Handling call messages.
%%
handle_call({get, Pool_name, Conn_pid}, _From, {Busy, Idle, DS_def} = State) ->
  case Idle of
    [] -> 
      case 
        C = conn:open_connection(
          DS_def#datasource.host, 
          DS_def#datasource.port, 
          DS_def#datasource.database, 
          DS_def#datasource.user, 
          DS_def#datasource.password, 
          DS_def#datasource.flags
        )
      of
        #mysql_error{} -> {reply, {stop, C}, State};
        _ ->
          Conn = C#connection{pool_name = Pool_name},
          gen_tcp:controlling_process(Conn#connection.socket, Conn_pid),
          {reply, {ok, Conn}, {[Conn | Busy], Idle, DS_def}}
      end;
    [Conn | New_idle] -> 
      gen_tcp:controlling_process(Conn#connection.socket, Conn_pid),
      {reply, {ok, Conn}, {[Conn | Busy], New_idle, DS_def}} 
  end;

handle_call(count, _From, {Busy, Idle, _DS_def} = State) ->
  {reply, {length(Busy), length(Idle)}, State};

handle_call({close, Conn}, _From, {Busy, Idle, DS_def}) ->
%  conn_sup:clean_children(DS_def#datasource.name),
  New_idle =
    case lists:member(Conn, Busy) of
      true -> [Conn | Idle];
      false -> Idle
    end,
  {reply, ok, {lists:delete(Conn, Busy), New_idle, DS_def}};

handle_call({remove, Conn}, _From, {Busy, Idle, DS_def}) ->
  {reply, ok, {lists:delete(Conn, Busy), lists:delete(Conn, Idle), DS_def}}; %% TODO: delete from Idle???

handle_call({dispose, Conn}, _From, {Busy, Idle, DS_def}) ->
  send_cmd:send_cmd_packet_no_resp(Conn, <<1:8>>),
  gen_tcp:close(Conn#connection.socket),
  {reply, ok, {lists:delete(Conn, Busy), lists:delete(Conn, Idle), DS_def}}; %% TODO: delete from Idle???

handle_call(destroy, _From, {Busy, Idle, DS_def}) ->
  F = fun(C) ->
    catch send_cmd:send_cmd_packet_no_resp(C, <<1:8>>),
    gen_tcp:close(C#connection.socket)
  end,
  lists:foreach(F, Busy),
  lists:foreach(F, Idle),
  {reply, ok, {[], [], DS_def}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}
.

%% @spec handle_cast(Request::term(), State::term()) -> Return
%% Return = {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%%
%% @doc Handling cast messages.
%%
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}
.

%% @spec handle_info(Info::term(), State::term()) -> Return
%% Return = {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%%
%% @doc Handling all non call/cast messages.
%%
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason::term(), State::term()) -> any() 
%% 
%% @doc Shutdown the server and returns any (ignored by gen_server).
%%
terminate(_Reason, _State) ->
%  io:format(user, " ### terminate conn_pool_srv ~p;~p~n", [_Reason, self()]),
  ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% 
%% @doc Converts process state when code is changed.
%%
code_change(_OldVsn, _State, _Extra) ->
  {ok, _State}.

%%
%%% Internal functions
%%

%% @spec name(N::atom()) -> atom() 
%% 
%% @doc Converts datasource name to connection pool name by adding '_pool' suffix.
%%
name(N) ->
  list_to_atom(lists:concat([N, "_", pool])).
