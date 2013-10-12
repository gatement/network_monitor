-module(nm_schema).

-include_lib("stdlib/include/qlc.hrl").
-include("nm_manager.hrl").

-compile(export_all).


%%
%% API Functions
%%

update() ->
	%% initialization functions
	update_base(),
	update_notification(),
	create_agent1(),

	%% following function is not for initialization
	%update_user_add_sport(),
	%update_note_last_updated(),
	ok.


update_base() ->
	mnesia:start(),

	% table schema
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	
	% table nm_session
	mnesia:create_table(nm_session, [{attributes, record_info(fields, nm_session)}, {disc_copies, [node()]}]),
	
	% table nm_setting
	mnesia:create_table(nm_setting, [{attributes, record_info(fields, nm_setting)}, {disc_copies, [node()]}]),
	
	% table nm_agent
	mnesia:create_table(nm_agent, [{attributes, record_info(fields, nm_agent)}, {disc_copies, [node()]}]),

	% table nm_agent_mib
	mnesia:create_table(nm_agent_mib, [{attributes, record_info(fields, nm_agent_mib)}, {disc_copies, [node()]}]),

	ok.


update_notification() ->
	% table nm_notification_subscriber
	mnesia:create_table(nm_notification_subscriber, [{attributes, record_info(fields, nm_notification_subscriber)}, {disc_copies, [node()]}]),

	ok.

	
create_agent1() ->
	% table nm_agent
	tools:sleep(10),
	Agent = model_agent:create(#nm_agent {user_id = my_user_1, target_name = "JohnsonPCPing", enabled = true}),
	AgentId = Agent#nm_agent.id,


	% table nm_agent_mib
	tools:sleep(10),
	AgentMib1 = model_agent_mib:create(#nm_agent_mib {agent_id = AgentId, oid = [1,3,6,1,4,1,10000,1,1,0], name = "johnsonlau.net", color = "#FF0000", type = ping, enabled = true}),
	
	tools:sleep(10),
	AgentMib2 = model_agent_mib:create(#nm_agent_mib {agent_id = AgentId, oid = [1,3,6,1,4,1,10000,1,2,0], name = "www.163.com", color = "#00FF00", type = ping, enabled = true}),


	% rrdtool db files
	rrdtool_helper:run_rrdtool_create_ping_cmd(AgentMib1#nm_agent_mib.id),
	rrdtool_helper:run_rrdtool_create_ping_cmd(AgentMib2#nm_agent_mib.id),

	ok.


%%
%% Local Functions
%%

