-module(page_notification_subscriber).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([out/1]).


%%
%% API Functions
%%

out(Arg) ->
	UserId = (Arg#arg.state)#arg_state.user_id,
	case model_user:is_admin(UserId) of
			true -> 
				Method = (Arg#arg.req)#http_request.method,
				if
					Method =:= 'POST' -> save(Arg);
					true -> {html, html(Arg)}
				end;
			false ->
				page_notfound:out(Arg)
	end.	


%%
%% Local Functions
%%

save(Arg) ->
	PostVals = yaws_api:parse_post(Arg),
	%io:format("~n~n~p~n~n",[PostVals]),
	IdString = tools:val_by_key("id", PostVals),
	Deleting = tools:val_by_key("deleting", PostVals),

	if
		Deleting =/= false -> % delete
			Id = erlang:list_to_integer(IdString),
			model_notification_subscriber:delete(Id);

		IdString =/= false -> % save
			Id = erlang:list_to_integer(IdString),
			Name = string:strip(tools:val_by_key("name", PostVals)),
			Email = string:strip(tools:val_by_key("email", PostVals)),
			WarningOnly = case tools:val_by_key("warning_only", PostVals) of
										false -> false;
										_ -> true
									end,
			Enabled = case tools:val_by_key("enabled", PostVals) of
										false -> false;
										_ -> true
									end,
			model_notification_subscriber:save(#nm_notification_subscriber{id = Id,
																											name = Name,
																											email = Email,
																											warning_only = WarningOnly,
																											enabled = Enabled});

		true -> % add
			Name = string:strip(tools:val_by_key("name_add", PostVals)),
			Email = string:strip(tools:val_by_key("email_add", PostVals)),
			WarningOnly = case tools:val_by_key("warning_only_add", PostVals) of
										false -> false;
										_ -> true
									end,
			Enabled = case tools:val_by_key("enabled_add", PostVals) of
										false -> false;
										_ -> true
								end,
			model_notification_subscriber:create(#nm_notification_subscriber{name = Name,
																											email = Email,
																											warning_only = WarningOnly,
																											enabled = Enabled})
	end,
	
	{html, html(Arg)}.


html(Arg) ->
	layout_site:header(Arg) ++
	"<div id=\"body\"><div id=\"subMenu\">" ++
	layout_notification:menu(Arg) ++
	"</div><div id=\"content\">" ++
	content_html(Arg) ++
	"</div></div>" ++
	layout_site:footer(Arg).


content_html(_Arg) ->
	Subscribers = model_notification_subscriber:get_all(),

	"<link type=\"text/css\" rel=\"stylesheet\" href=\"/css/page_notification_subscriber.css\" />
	<div class=\"navigationLocation\">Notification >> Subscriber</div>
	<table border =\"1\" cellspacing=\"0\">
		<tr>
			<td>name</td>
			<td>email</td>
			<td>warning only</td>
			<td>enabled</td>
			<td>deleting</td>
			<td>action</td>
		</tr>"
		 ++ get_subscriber_html(Subscribers, "") ++
		"<tr>
			<form method =\"post\">
			<td><input class=\"notificationSubscriberNameField\" name=\"name_add\" /></td>
			<td><input class=\"notificationSubscriberEmailField\" name=\"email_add\" /></td>
			<td><input type=\"checkbox\" name=\"warning_only_add\" value=\"true\" /></td>
			<td><input type=\"checkbox\" name=\"enabled_add\" value=\"true\" /></td>
			<td></td>
			<td><input type=\"submit\" value=\" Add \" /></td>
			</form>
		</tr>
	</table>".



get_subscriber_html([], Acc) ->
	Acc;

get_subscriber_html([H|T], Acc) ->
	WarningOnlyHtml = case H#nm_notification_subscriber.warning_only of
										true -> " checked=\"checked\"";
										false -> ""
									end,
	EnabledHtml = case H#nm_notification_subscriber.enabled of
										true -> " checked=\"checked\"";
										false -> ""
									end,
	Html = "<tr>
					<form method =\"post\">
					<td>
						<input type=\"hidden\" name=\"id\" value=\"" ++ erlang:integer_to_list(H#nm_notification_subscriber.id) ++ "\" />
						<input class=\"notificationSubscriberNameField\" name=\"name\" value=\"" ++ re:replace(H#nm_notification_subscriber.name, "\"", "\\&quot;", [global, {return,list}]) ++ "\" /></td>
					<td><input class=\"notificationSubscriberEmailField\" name=\"email\" value=\"" ++ re:replace(H#nm_notification_subscriber.email, "\"", "\\&quot;", [global, {return,list}]) ++ "\" /></td>
					<td><input type=\"checkbox\" name=\"warning_only\" value=\"true\"" ++ WarningOnlyHtml ++ " /></td>
					<td><input type=\"checkbox\" name=\"enabled\" value=\"true\"" ++ EnabledHtml ++ " /></td>
					<td><input type=\"checkbox\" name=\"deleting\" value=\"true\" /></td>
					<td><input type=\"submit\" value=\"Save\" /></td>
					</form>
				</tr>",
	get_subscriber_html(T, Html ++ Acc).

	
	
