-module(network_monitor_supervisor).
-behaviour(supervisor).

-export([start/0, init/1]).


%%
%% API Functions
%%

start() ->
	spawn(fun() ->
			supervisor:start_link(?MODULE, [])
	end).


init([]) ->
	{ok, {{one_for_one, 30, 10},
			[
			 {network_monitor,
			 {network_monitor, start, []},
			 permanent,
			 10000,
			 worker,
			 [network_monitor]}
			]
		 }
	}.
	