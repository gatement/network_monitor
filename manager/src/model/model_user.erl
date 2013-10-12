-module(model_user).
-include("nm_manager.hrl").
-export([get/1,
		get/2,
		auth/2,
		auth/3,
		get_role/1,
		get_role/2,
		is_admin/1,
		is_admin/2]).


%% ===================================================================
%% API functions
%% ===================================================================

% return undefined/{Password, Role, Status}
get(UserId) ->
	model_user:get(UserId, get_users_file_name()).

get(UserId, UsersFile) ->
    {ok, UserInfos} = file:consult(UsersFile),
	proplists:get_value(UserId, UserInfos).


% return false/disabled/#nm_user
auth(Id, Pwd) ->
	auth(Id, Pwd, get_users_file_name()).

auth(Id, Pwd, UsersFile) ->
	case model_user:get(Id, UsersFile) of
		undefined -> false;
		{Password, Role, Status} ->
			case Status of
				disabled -> disabled;
				enabled ->
					if
						Password =:= Pwd -> #nm_user{
													id = Id, 
													password = Password, 
													role = Role, 
													status = Status};
						true -> false
					end
			end
	end.


% return admin/guest
get_role(UserId) ->
	get_role(UserId, get_users_file_name()).

get_role(UserId, UsersFile) ->
	{_Password, Role, _Status} = model_user:get(UserId, UsersFile),
	Role.


% return true/false
is_admin(UserId) ->
	is_admin(UserId, get_users_file_name()).

is_admin(UserId, UsersFile) ->
	case get_role(UserId, UsersFile) of
			admin -> true;
			_ -> false
	end.


%% ===================================================================
%% Local Functions
%% ===================================================================
get_users_file_name() ->
	"../users.conf".
