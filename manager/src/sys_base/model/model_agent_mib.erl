-module(model_agent_mib).

-include("nm_manager.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([create/1, 
			save/1,
			delete/1,
			get/1,
			get_by_agent_id/1,
			get_active_by_agent_id/1]).


%%
%% API Functions
%%

create(AgentMib) ->
	AgentMib2 = AgentMib#nm_agent_mib{id = tools:now_big_integer()},
	Fun = fun() ->
			mnesia:write(AgentMib2)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> AgentMib2;
		_ -> error
	end.

	
save(AgentMib) ->
	Fun = fun() ->
			mnesia:write(AgentMib)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


delete(Id) ->
	Fun = fun() -> 
		mnesia:delete({nm_agent_mib, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		{aborted, _} -> error
	end.
	

get(Id) ->
	Fun = fun() -> 
		mnesia:read({nm_agent_mib, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, [AgentMib]} -> AgentMib;
		{aborted, _} -> error
	end.


get_by_agent_id(AgentId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_agent_mib),
									X#nm_agent_mib.agent_id =:= AgentId]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	Result.


get_active_by_agent_id(AgentId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_agent_mib),
								X#nm_agent_mib.agent_id =:= AgentId,
								X#nm_agent_mib.enabled =:= true]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	Result.


%%
%% Local Functions
%%

