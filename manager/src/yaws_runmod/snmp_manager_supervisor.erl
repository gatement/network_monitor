-module(snmp_manager_supervisor).
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
	{ok, {{one_for_one, 3, 10},
			[
			 {snmp_manager,
			 {snmp_manager, start, []},
			 permanent,
			 10000,
			 worker,
			 [snmp_manager]}
			]
		 }
	}.

