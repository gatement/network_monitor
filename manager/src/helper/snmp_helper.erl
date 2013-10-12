-module(snmp_helper).
-export([get_agent_name/2,
		get_agent_desc/2,
		get_oid_val/3]).

-include("nm_manager.hrl").

%%
%% API Functions
%%

% return Name or error
get_agent_name(UserId, TargetName) ->	
	application:start(snmp),
	case snmpm:sync_get(UserId, TargetName, [[1,3,6,1,2,1,1,5,0]], 20000) of
		{ok,{noError,0,[{varbind,[1,3,6,1,2,1,1,5,0],'OCTET STRING',Name,1}]},_} -> Name;
		_ -> error
	end.
	
	
% return Desc or error
get_agent_desc(UserId, TargetName) ->	
	application:start(snmp),
	case snmpm:sync_get(UserId, TargetName, [[1,3,6,1,2,1,1,1,0]], 20000) of
		{ok,{noError,0,[{varbind,[1,3,6,1,2,1,1,1,0],'OCTET STRING',Desc,1}]},_} -> Desc;
		_ -> error
	end.
	

% return Value or error
get_oid_val(UserId, TargetName, Oid) ->	
	application:start(snmp),
	case snmpm:sync_get(UserId, TargetName, [Oid], 1200000) of
		{ok,{noError,0,[{varbind,_,_,Value,1}]},_} -> Value;
		_ -> error
	end.


%%
%% Local Functions
%%

