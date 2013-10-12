-module(model_session).

-include("nm_manager.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%

-export([create/1, 
		delete/1, 
		delete_by_user/1, 
		update_socket/3, 
		get/1, 
		exist/1, 
		update_last_active/1, 
		clear_old/1, 
		clear_socket/1,
		get_socket_pid_by_socket_territory/1]).


%%
%% API Functions
%%

create(Session) ->
	RandomStr = tools:random_string(15),
	Id = tools:generate_id(Session#nm_session.user_id) ++ RandomStr,
	LastActive = erlang:now(),

	Fun = fun() ->
			mnesia:write(Session#nm_session{id = Id,
							     last_active = LastActive})	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> Id;
		_ -> error
	end.


delete(Id) ->
	Fun = fun() -> 
		mnesia:delete({nm_session, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		{aborted, _} -> error
	end.


delete_by_user(UserId) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_session), 
						X#nm_session.user_id =:= UserId]))
	end,
	{atomic, Sessions} = mnesia:transaction(Fun),

	DelFun = fun(Session) -> 
			model_session:delete(Session#nm_session.id) 
	end,
	lists:foreach(DelFun, Sessions),

	ok.


get(Id) ->
	Fun = fun() -> 
		mnesia:read({nm_session, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, Session} -> Session;
		_ -> error
	end.


exist(Id) ->
	Fun = fun() -> 
		mnesia:read({nm_session, Id})
	end,

	case mnesia:transaction(Fun) of
		{atomic, []} -> false;
		{atomic, [_Session]} -> true
	end.


update_socket(Id, SocketPid, SocketTerritory) ->
	case model_session:get(Id) of
		[] ->
			error;
		[Session] ->
			Fun = fun() ->
					mnesia:write(Session#nm_session{socket_pid = SocketPid,
									                socket_territory = SocketTerritory})	  
			end,

			mnesia:transaction(Fun)
	end.


update_last_active(Id) ->
	case model_session:get(Id) of
		[] -> 
			error;
		[Session] ->
			Fun = fun() ->
					mnesia:write(Session#nm_session{last_active = erlang:now()})	  
			end,

			mnesia:transaction(Fun)
	end.

%ttl is the max seconds a session can live 
%ttl needs to less than 7 days
clear_old(Ttl) ->
	{A, S, C} = erlang:now(),
	B = S - Ttl,
	OldTime = {A, B, C},

	Fun = fun() -> 
		Sessions = qlc:e(qlc:q([X || X <- mnesia:table(nm_session), 
						X#nm_session.last_active < OldTime])),

		DelFun = fun(Session) -> 
			model_session:delete(Session#nm_session.id) 
		end,
		lists:foreach(DelFun, Sessions)
	end,

	mnesia:transaction(Fun),

	ok.

clear_socket(SocketPid) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_session), 
						X#nm_session.socket_pid =:= SocketPid]))
	end,
	{atomic, Sessions} = mnesia:transaction(Fun),

	DelFun = fun(Session) -> 
		model_session:update_socket(Session#nm_session.id, undefined, undefined) 
	end,
	lists:foreach(DelFun, Sessions),

	ok.


get_socket_pid_by_socket_territory(SocketTerritory) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X#nm_session.socket_pid || X <- mnesia:table(nm_session), 
						X#nm_session.socket_territory =:= SocketTerritory]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),
	Result.


