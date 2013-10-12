-module(nm_cleanup_supervisor).
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
			 {nm_cleanup,
			 {nm_cleanup, start, []},
			 permanent,
			 10000,
			 worker,
			 [nm_cleanup]}
			]
		 }
	}.

