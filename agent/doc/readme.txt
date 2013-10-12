1. Files need to take care
- mibs/PING-MIB.mib
- mibs/PING-MIB.funcs
- priv/mibs/PING-MIB.bin (compile from "mibs/PING-MIB.mib" by "erlc")
- src/ping_instrumentation.erl (need compile)
- ping_targets.config
- priv/conf/agent.conf
- priv/conf/standard.conf
- snmp.config
- start.bat (start.sh)

2. Build the source code 
Go to the networkmonitor root folder, open an erlang(erl) session and then type "make:all()."
