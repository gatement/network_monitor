-module(nm_schema).
-include_lib("stdlib/include/qlc.hrl").
-include("nm_manager.hrl").
-export([update/0]).


%% ===================================================================
%% API functions
%% ===================================================================

update() ->
	%% initialization functions
	update_base(),
	update_notification(),
	update_ping(),

	%% update functions

	ok.


%% ===================================================================
%% Local Functions
%% ===================================================================

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

	ok.


update_notification() ->
	% table nm_notification_subscriber
	mnesia:create_table(nm_notification_subscriber, [{attributes, record_info(fields, nm_notification_subscriber)}, {disc_copies, [node()]}]),

	ok.

	
update_ping() ->
	% table nm_agent_ping
	mnesia:create_table(nm_agent_ping, [{attributes, record_info(fields, nm_agent_ping)}, {disc_copies, [node()]}]),

	ok.
