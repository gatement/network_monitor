-module(model_agent).

-include("nm_manager.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([create/1, 
			save/1,
			get/1,
			delete/1,
			get_all/0,
			get_all_active/0,
			get_any_id/0]).


%%
%% API Functions
%%

create(Agent) ->
	Agent2 = Agent#nm_agent{id = tools:now_big_integer()},
	Fun = fun() ->
			mnesia:write(Agent2)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Agent2;
		_ -> error
	end.

	
save(Agent) -> 	
	Fun = fun() ->
			mnesia:write(Agent)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.
	

get(Id) ->
	Fun = fun() -> 
		mnesia:read({nm_agent, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, [Agent]} -> Agent;
		{aborted, _} -> error
	end.
	

get_any_id() ->
	LastAgent = lists:last(get_all()),
	LastAgent#nm_agent.id.


delete(Id) ->
	Fun = fun() -> 
		% delete all nm_agent_mib as well
		Mibs = model_agent_mib:get_by_agent_id(Id),
		lists:foreach(fun(Mib) -> mnesia:delete({nm_agent_mib, Mib#nm_agent_mib.id}) end, Mibs),

		mnesia:delete({nm_agent, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		{aborted, _} -> error
	end.


get_all() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_agent)]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	Result.


get_all_active() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_agent),
							X#nm_agent.enabled =:= true]))
	end,
	
	{atomic, Result} = mnesia:transaction(Fun),
	Result.


%%
%% Local Functions
%%

