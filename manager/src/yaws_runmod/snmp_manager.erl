-module(snmp_manager).
			
-include("nm_manager.hrl").

-export([start/0,
			get_mib_val/2]).


%%
%% API Functions
%%

start() ->
	application:start(snmp),
	tools:sleep(2000),
	do().


%%
%% Local Functions
%%

do() ->
	Agents = model_agent:get_all_active(),
	
	Func = fun(Agent) ->	
		Mibs = model_agent_mib:get_active_by_agent_id(Agent#nm_agent.id),
		lists:foreach(fun(Mib) ->  get_mib_val(Agent, Mib) end, Mibs)
	end,

	lists:foreach(Func, Agents),

	tools:sleep(300000), % in milliseconds, 300000 = 5 min
	do().


get_mib_val(Agent, Mib) ->
	Val = snmp_helper:get_oid_val(Agent#nm_agent.user_id, Agent#nm_agent.target_name, Mib#nm_agent_mib.oid),
	case Mib#nm_agent_mib.type of
		ping -> 
			[Loss, Time] = string:tokens(Val, ","),
			rrdtool_helper:run_rrdtool_update_ping_cmd(Mib#nm_agent_mib.id, Loss, Time),
			nm_event:got_mib_val(ping, Agent, Mib, Val)
	end.
