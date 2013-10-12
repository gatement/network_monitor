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

%% @since 2011-08-15
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc The module implements supervisor behaviour to manage a pool of connections. 
%% The supervisor controls a childs of two type: multiple connection gen_servers and
%% one connection pool gen_server that keeps reusable connection objects.
%%

-module(conn_sup).
-behaviour(supervisor).

%%
%% Include files
%%
-include("my.hrl").

%%
%% External exports
%%
-export([
	start_link/2,
	start_child/1,
	stop_child/1,
	stop_child/2
]).

%%
%% Internal exports. (gen_supervisor callbacks)
%%
-export([
	init/1
]).

%%
%% API Functions
%%

%% @spec start_link(DS_name::atom(), DS_def::#datasource{}) -> Result
%% Result = {ok,Child} | {ok,Child,Info} | {error,Error}
%% Child = pid() | undefined
%% Info = term()
%% Error = already_present | {already_started,Child} | term()
%%
%% @doc Creates and starts gen_supervisor that implements datasource or connection pool.
%% During starting it creates a child named connection pool that keeps reusable
%% connection objects.
%%
start_link(DS_name, DS_def) -> 
%	io:format(user, " >>> conn_sup:start_link/2: ~p ~p~n", [DS_name, DS_def]),
	R = supervisor:start_link({local, DS_name}, ?MODULE, []),
	Child_spec = {
		connection_pool,
		{conn_pool_srv, start_link, [DS_name, DS_def]}, %% MFA of the child process
		temporary, brutal_kill, worker,
		[conn_pool_srv]
	},
	{ok, _} = supervisor:start_child(DS_name, Child_spec),
	R
.

%% @spec start_child(DS_name::atom()) -> Result
%% Result = {ok, Child} | {ok, Child, Info} | {error, Error}
%% Child = pid() | undefined
%% Info = term()
%% Error = already_present | {already_started, Child} | term()
%%
%% @doc Starts connection gen_server. During initiation server tryes to reuse existed
%% connections but if idle connection list is empty the new connection is craeted.
%%
start_child(DS_name) ->
%	io:format(user, " >>> conn_sup:start_child/1~n", []),
	Reference = list_to_atom(erlang:ref_to_list(make_ref())),
	Child_spec = {
		Reference, %% connection unique reference 
		{conn_srv, open_connection, [{DS_name, Reference}]}, %% MFA of the child process
		temporary, brutal_kill, worker,
		[conn_srv]
	},
	supervisor:start_child(DS_name, Child_spec)
.

stop_child(Conn_pid) -> stop_child(Conn_pid, false)
.

stop_child(Conn_pid, Destroy) ->
	Alive = is_process_alive(Conn_pid),
	if
		Alive ->
%			io:format(user, " ---> conn_sup:stop_child/1 ~p, ~p~n", [Conn_pid,Destroy]),
			{DS_name, Ref, Connection_rec} =
				gen_server:call(Conn_pid, connection_state, ?GEN_SERVER_TIMEOUT),
			ok = gen_server:call(Conn_pid, close_connection, ?GEN_SERVER_TIMEOUT),
			_R = supervisor:delete_child(DS_name, Ref),
%			io:format(user, "     stop child return: ~p~n", [_R])
			case Destroy of
				true -> conn_pool_srv:dispose(Connection_rec);
				false -> conn_pool_srv:close(Connection_rec)
			end;
		true -> ok
	end
.

%%
%% Server callback functions
%%

%% @spec init(Arg::term()) -> {ok, {SupFlags, []}}
%%
%% @doc Initiates the supervisor.
%%
init(_) ->
%	io:format(user, " >>> conn_sup:init/1 pid: ~p~n", [self()]),
	{ok,
		{	{one_for_one, 0, 1}, %% Restart strategy
			[]
		}
	}
.
