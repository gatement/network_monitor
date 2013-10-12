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

%% @since 2011-08-16
%% @copyright 2010-2012 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc The module implements application behaviour to manage a mySQL client. 
%%

-module(my_app).
-behaviour(application).

%%
%% Internal exports. (gen_supervisor callbacks)
%%
-export([
	start/2,
	prep_stop/1,
	stop/1
]).

%%
%% Server callback functions
%%

%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
%% @spec start(Type::term(), Args::list()) -> Return
%% Return = {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% 
%% @doc Starts the root supervisor of the client.
%%
start(_Type, []) ->
%	io:format(user, " >>> my_app:start/2~n", []),
	datasource_sup:start_link()
.

%% @spec stop(State::term()) -> any() 
%% 
%% @doc This is callback after application shut down.
%%
stop(_State) ->
%	io:format(user, " <<< my_app:stop/1 ~p~n", [_State]),
	ok
.

%% @spec prep_stop(A::term()) -> any() 
%% 
%% @doc this is callback before application is starting shut down. It closes all
%% datasources of application.
%%
prep_stop(_A) ->
%	io:format(user, " <<< my_app:prep_stop/1 ~p~n", [_A]),
%	io:format(user, "     children list:~p~n", [supervisor:which_children(datasource_sup)]),
	F = fun({Name, _, _, _}) -> my:close_datasource(Name) end,
	lists:foreach(F, supervisor:which_children(datasource_sup))
.
