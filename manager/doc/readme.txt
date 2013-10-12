1. Files need to take care
- snmp.config
- start.bat (start.sh)
- start_agent_tester.bat (optional)
- users.config
- yaws.conf
- nm_schema.erl (need compile)
- priv/conf/manager.conf
- priv/conf/users.conf
- priv/conf/agents.conf

2. Build the source code 
Go to the networkmonitor root folder, open an erlang(erl) session and then type "make:all()."

3. Setup database
Startup the site with interactive mode, type "nm_schema:update()". 

4. Open the site and config it
