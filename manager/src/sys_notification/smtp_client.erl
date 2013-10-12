-module(smtp_client).
-compile(export_all).


test(From, FromPwd) ->
	send_email(From, FromPwd, ["gatement@163.com", "gatement@gmail.com", "70526622@qq.com"], "Network Monitor Statistics (2012-4-12)", 
		"<html><body><table border=\"1\"  cellspacing=\"0\">"
		"<tr><td><strong>Time</strong></td><td><strong>Bytes</strong></td><td><strong>Count</strong></td><td><strong>From</strong></td><td><strong>To</strong></td><td><strong>Loss</strong></td><td><strong>Time</strong></td></tr>"
		"<tr><td>2012-4-12 13:45:23</td><td>64</td><td>12</td><td>192.168.1.2</td><td>192.168.35.35 (Dev)</td><td style=\"color: #FF0000;\"><strong>23%</strong></td><td style=\"color: #FF0000;\"><strong>545ms</strong></td></tr>"
		"<tr><td>2012-4-12 13:45:23</td><td>64</td><td>12</td><td>192.168.1.2</td><td>192.168.35.35 (Dev)</td><td>2%</td><td>345ms</td></tr>"
		"</table></body></html>").


send_email(From, FromPwd, To, Subject, Content) ->
	io:format("~n======== start sending email ========~n"),

	application:start(crypto),
	application:start(public_key),
	application:start(ssl),

	tools:sleep(2000),

	case ssl:connect("smtp.gmail.com", 465, [{active, false}], 60000) of
	    {ok, Socket} ->
		    recv(Socket),
		    send(Socket, "HELO johnsonlau.net"),
		    send(Socket, "AUTH LOGIN"),
		    send(Socket, binary_to_list(base64:encode(From))),
		    send(Socket, binary_to_list(base64:encode(FromPwd))),
		    send(Socket, "MAIL FROM: <" ++ From ++ ">"),
		    lists:foreach(fun(Mail) -> smtp_client:send(Socket, "RCPT TO:<" ++ Mail ++ ">") end, To),
		    send(Socket, "DATA"),
		    send_no_receive(Socket, "MIME-Version: 1.0"),
		    send_no_receive(Socket, "Content-Type: text/html; charset=UTF-8"),
		    send_no_receive(Socket, "From: <" ++ From ++ ">"),
		    lists:foreach(fun(Mail) -> send_no_receive(Socket, "To: <" ++ Mail ++ ">") end, To),
		    send_no_receive(Socket, "Date: " ++ get_time()),
		    send_no_receive(Socket, "Subject: " ++ Subject),
		    send_no_receive(Socket, ""),
		    send_no_receive(Socket, Content),
		    send_no_receive(Socket, ""),
		    send(Socket, "."),
		    send(Socket, "QUIT"),
		    ssl:close(Socket),
			ok;
		{error, Reason} -> 
			io:format("Mail error: ~p~n", [Reason]),
			error
	end,

	io:format("======== end sending email ========~n"),
	io:format("(~s)~n~n", [tools:datetime_string()]).


%%
%% Local Functions
%%

get_time() -> 
	{{Year,Month,Day},{Hour,Min,Sec}} = erlang:universaltime(),
	io_lib:format("~p-~p-~p ~p:~p:~p +0000", [Year,Month,Day,Hour,Min,Sec]).


send_no_receive(Socket, Data) ->
	io:format("Mail C: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n").


send(Socket, Data) ->
	io:format("Mail C: ~p~n", [Data]),
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).


recv(Socket) ->
    case ssl:recv(Socket, 0, 60000) of
		{ok, _Return} -> 
			io:format("Mail S: ~p~n", [_Return]),
			ok;
		{error, _Reason} -> 
			io:format("Mail error: ~p~n", [_Reason]),
			error
    end.
