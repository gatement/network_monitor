-module(gmail_client_tests).
-export([test/3]).

%% ===================================================================
%% API functions
%% ===================================================================

test(From, FromPwd, Receivers) ->
	Subject = io_lib:format("Gmail Test Email (~s)", [tools:datetime_string('yyyy-MM-dd hh:mm:ss')]),
	Body = "<html><body><table border=\"1\"  cellspacing=\"0\">"
		"<tr><td><strong>Time</strong></td><td><strong>Bytes</strong></td><td><strong>Count</strong></td><td><strong>From</strong></td><td><strong>To</strong></td><td><strong>Loss</strong></td><td><strong>Time</strong></td></tr>"
		"<tr><td>2012-4-12 13:45:23</td><td>64</td><td>12</td><td>192.168.1.2</td><td>192.168.35.35 (Dev)</td><td style=\"color: #FF0000;\"><strong>23%</strong></td><td style=\"color: #FF0000;\"><strong>545ms</strong></td></tr>"
		"<tr><td>2012-4-12 13:45:23</td><td>64</td><td>12</td><td>192.168.1.2</td><td>192.168.35.35 (Dev)</td><td>2%</td><td>345ms</td></tr>"
		"</table></body></html>",

	gmail_client:send_email(From, FromPwd, Receivers, Subject, Body).
	

%% ===================================================================
%% Local Functions
%% ===================================================================



