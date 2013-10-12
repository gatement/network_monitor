-module(agent).

-export([start/0, stop/0]).

start() ->
	application:start(snmp),
	snmpa:load_mibs(snmp_master_agent, ["../priv/mibs/PING-MIB"]).

stop() ->
	application:stop(snmp).
