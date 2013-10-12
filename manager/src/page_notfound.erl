-module(page_notfound).

-include("yaws_api.hrl").

-export([out/1, out404/3]).


%%
%% API Functions
%%

out(_Arg) ->
	[{status, 404},
	{ehtml,
		{html, [],
			[
				{h1,[], "Not Allowed"},
		 		{p, [], "The requested URL was not allowed."},
				{a, [{href, "/"}], "home"}
			]
		}
	}].

out404(_Arg, _GC, _SC) ->
	[{status, 404},
	{ehtml,
		{html, [],
			[
				{h1,[], "Not Found"},
		 		{p, [], "The requested URL was not found."},
				{a, [{href, "/"}], "home"}
			]
		}
	}].

