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

%% @since 2011-08-18
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc The module implements supervisor behaviour to manage a set of datasources. 
%%

-module(datasource_sup).
-behaviour(supervisor).

%%
%% External exports
%%
-export([
	start_link/0
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

%% @spec start_link() -> Result
%% Result = {ok,Child} | {ok,Child,Info} | {error,Error}
%% Child = pid() | undefined
%% Info = term()
%% Error = already_present | {already_started,Child} | term()
%%
%% @doc Creates and starts gen_supervisor that manages datasources (or connection pools).
%%
start_link() -> 
%	io:format(user, " >>> datasource_sup:start_link/0~n", []),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [])
.

%%
%% Server callback functions
%%

%% @spec init(Arg::term()) -> {ok, {SupFlags, []}}
%%
%% @doc Initiates the supervisor.
%%
init(_) ->
	{ok,
		{	{one_for_one, 0, 1}, %% Restart strategy
			[]
		}
	}
.
