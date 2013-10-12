-module(model_notification_subscriber).

-include("nm_manager.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([create/1, 
			save/1,
			delete/1,
			get_all/0,
			get_enabled/0,
			get_enabled_not_warning_only/0]).


%%
%% API Functions
%%

create(Subscriber) ->
	Fun = fun() ->
			mnesia:write(Subscriber#nm_notification_subscriber{id = tools:now_big_integer()})	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.

	
save(Subscriber) -> 	
	Fun = fun() ->
			mnesia:write(Subscriber)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


delete(Id) ->
	Fun = fun() -> 
		mnesia:delete({nm_notification_subscriber, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		{aborted, _} -> error
	end.


get_all() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_notification_subscriber)]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	Result.


get_enabled() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_notification_subscriber),
							X#nm_notification_subscriber.enabled =:= true]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	Result.


get_enabled_not_warning_only() ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_notification_subscriber),
							X#nm_notification_subscriber.enabled =:= true,
							X#nm_notification_subscriber.warning_only =:= false]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	Result.


%%
%% Local Functions
%%

