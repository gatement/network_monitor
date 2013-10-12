-module(agent_supervisor).
-behaviour(supervisor).
-export([start/0,
		init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	spawn(fun() ->
			supervisor:start_link(?MODULE, [])
	end).

	
init([]) ->
	{ok, {{one_for_one, 3, 10},
			[
			 {agent,
			 {agent, start, []},
			 permanent,
			 10000,
			 worker,
			 [agent]}
			]
		 }
	}.

	
%% ===================================================================
%% Local Functions
%% ===================================================================
	