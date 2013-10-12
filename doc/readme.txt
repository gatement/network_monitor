- Config: 
   copy "doc/networkmonitor-sample.cfg" and rename it to "networkmonitor.cfg" in networkmonitor root folder then update the settings in it

- Build: 
   go to the app root folder, open a erlang(erl) session and then type "make:all()."

- Starting sample scripts:
   "doc/start-sample.bat": Window start script, is sutiable for configuring it as a service by Window schedule tasks
   "doc/start-sample.sh": Unix/Linux start script
