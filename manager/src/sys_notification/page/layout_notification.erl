-module(layout_notification).

-include("yaws_api.hrl").
-include("nm_manager.hrl").

-export([menu/1]).


%%
%% API Functions
%%

menu(_Arg) ->
	"<div><a href=\"/notification/setting\">Setting</a></div>
	<div><a href=\"/notification/subscriber\">Subscriber</a></div>".

