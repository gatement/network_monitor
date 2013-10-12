-module(nm_event).

-include("nm_manager.hrl").

-export([got_mib_val/4]).


%%
%% API Functions
%%

got_mib_val(ping, Agent, Mib, Val) ->
	notification_email_handler:handle_ping(Agent, Mib, Val);

got_mib_val(_Type, _Agent, _Mib, _Val) ->
	not_implement_yet.

%%
%% Local Functions
%%


