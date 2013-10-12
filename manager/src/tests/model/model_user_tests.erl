-module(model_user_tests).
-include_lib("eunit/include/eunit.hrl").
-include("nm_manager.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

get_test_() ->
	{Password1, Role1, Status1} = model_user:get("admin", get_users_test_file_name()),
	{Password2, Role2, Status2} = model_user:get("guest", get_users_test_file_name()),
	
	[
		?_assertEqual(Password1, "admin")
		,?_assertEqual(Role1, admin)
		,?_assertEqual(Status1, enabled)
		,?_assertEqual(Password2, "123456")
		,?_assertEqual(Role2, guest)
		,?_assertEqual(Status2, disabled)
	].


auth_test_() ->
	Result1 = model_user:auth("admin", "123456", get_users_test_file_name()),
	Result2 = model_user:auth("admin", "admin", get_users_test_file_name()),
	Result3 = model_user:auth("guest", "123456", get_users_test_file_name()),

	[
		?_assert(Result1 =:= false)
		,?_assert(erlang:is_record(Result2, nm_user))
		,?_assert(Result3 =:= disabled)
	].


get_role_test_() ->
	Result1 = model_user:get_role("admin", get_users_test_file_name()),
	Result2 = model_user:get_role("guest", get_users_test_file_name()),
	
	[
		?_assert(Result1 =:= admin)
		,?_assert(Result2 =:= guest)
		,?_assertError(_, model_user:get_role("noexist", get_users_test_file_name()))
	].


is_admin_test_() ->
	Result1 = model_user:is_admin("admin", get_users_test_file_name()),
	Result2 = model_user:is_admin("guest", get_users_test_file_name()),
	
	[
		?_assert(Result1 =:= true)
		,?_assert(Result2 =:= false)
		,?_assertError(_, model_user:is_admin("noexist", get_users_test_file_name()))
	].

	

%% ===================================================================
%% Local Functions
%% ===================================================================

get_users_test_file_name() ->
	"../users_test.conf".

