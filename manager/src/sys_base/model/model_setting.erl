-module(model_setting).

-include("nm_manager.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([get/1, 
			set/1]).


%%
%% API Functions
%%

get(Key) ->
	Fun = fun() -> 
		qlc:e(qlc:q([X || X <- mnesia:table(nm_setting), 
								X#nm_setting.key =:= Key]))
	end,

	{atomic, Result} = mnesia:transaction(Fun),

	case Result of
		[] -> "";
		[Setting] -> Setting#nm_setting.value
	end.


set(Setting) ->
	Fun = fun() ->
			mnesia:write(Setting)	  
	end,

	case mnesia:transaction(Fun) of
		{atomic, ok} -> ok;
		_ -> error
	end.


%%
%% Local Functions
%%

