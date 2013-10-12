-module(model_user).

-include("nm_manager.hrl").

-export([get/1,
			auth/2,
			get_role/1,
			is_admin/1]).

% return undefined/{Password, Role, Status}
get(UserId) ->
    {ok, [UserInfos]} = file:consult("../users.config"),
	proplists:get_value(UserId, UserInfos).

% return false/disabled/#nm_user
auth(Id, Pwd) ->
	case model_user:get(Id) of
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
	{_Password, Role, _Status} = model_user:get(UserId),
	Role.

% return true/false
is_admin(UserId) ->
	case model_user:get_role(UserId) of
			admin -> true;
			_ -> false
	end.
